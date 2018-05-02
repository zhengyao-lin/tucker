module Tucker.P2P.Scheduler where

import Data.List
import qualified Data.Set as SET

import Control.Monad
import Control.Concurrent

import Tucker.Atom
import Tucker.Util
import Tucker.Thread
import qualified Tucker.Lock as LK

import Tucker.P2P.Node

-- task scheduler
data Scheduler t =
    Scheduler {
        sched_tid   :: ThreadId,
        var_lock    :: LK.Lock,
        blacklist   :: Atom (SET.Set Node),
        cur_assign  :: Atom [(Node, t)],
        sched_alive :: Atom Bool
    }

{-

a scheduler recevies the assignment

it waits for the timeout, and if after timeout the assignment list is not empty,
it finds the slow nodes that haven't responded during the wait

it extracts the tasks for the slow nodes, and pass the task list to an user
handler which will then return a new assignment list composed of the undone tasks

the scheduler then
    1. puts the slow nodes to blacklist
    2. updates the assignment list
    3. goes into the next loop

note that the scheduler runs in a different thread, so newScheduler is non-blocking
user can use the Scheduler handler to control the scheduling(e.g. removing the task when one is done)

-}

filterAssign :: Scheduler t -> ((Node, t) -> Bool) -> IO Bool
filterAssign (Scheduler {
    var_lock = lock,
    cur_assign = assign_var
}) pred = LK.with lock $ do
    orig_assign <- getA assign_var
    new_assign <- appA (filter pred) assign_var
    return $ length orig_assign /= length new_assign

-- remove a task from a scheduler
-- return if one or more tasks are removed
removeTask :: (Eq t, NodeTask t) => Scheduler t -> t -> IO Bool
removeTask sched task = filterAssign sched ((/= task) . snd)

-- remove a node from the assignment list
removeNode :: Scheduler t -> Node -> IO Bool
removeNode sched node = filterAssign sched ((/= node) . fst)

-- cancel a scheduler
cancel :: Scheduler t -> IO ()
cancel (Scheduler {
    sched_tid = tid,
    sched_alive = alive
}) = do
    orig_alive <- appA_ (const False) alive
    when orig_alive (killThread tid)

clearBlacklist :: Scheduler t -> IO ()
clearBlacklist (Scheduler {
    var_lock = lock,
    blacklist = blacklist
}) =
    LK.with lock $ setA blacklist SET.empty

-- remove a node from the blacklist
-- NOTE: this will not decrease the blacklist count
removeFromBlacklist :: Scheduler t -> Node -> IO ()
removeFromBlacklist (Scheduler {
    var_lock = lock,
    blacklist = blacklist
}) node =
    LK.with lock $
        appA (SET.delete node) blacklist >> return ()

newScheduler :: NodeTask t
             => MainLoopEnv -> Int
             -> (Scheduler t -> IO [(Node, t)])
             -> (Scheduler t -> [t] -> [Node] -> IO [(Node, t)])
             -> (Scheduler t -> [t] -> IO [(Node, t)])
             -> IO (Scheduler t)
newScheduler env timeout_s init_assign reassign failed_reassign = do
    let timeout_ms = fi $ timeout_s * 1000

    var_lock <- LK.new

    blacklist_var <- newA SET.empty
    assign_var <- newA []

    alive_var <- newA True

    let tmp_sched = Scheduler {
            sched_tid = undefined,
            var_lock = var_lock,
            blacklist = blacklist_var,
            cur_assign = assign_var,
            sched_alive = alive_var
        }

        watch = do
            tid <- myThreadId
            let sched = tmp_sched { sched_tid = tid }
    
            timeout_ms_var <- newA timeout_ms

            forever $ do
                start_time <- unixTimestamp

                -- tLnM "start waiting"

                timeout_ms <- getA timeout_ms_var
                msDelay timeout_ms

                -- delays <- envAllNetDelay env
                -- tLnM $ "median " ++ show (median delays) ++
                --        "ms, average " ++ show (average delays) ++
                --        "ms, last " ++ show (last (sort delays)) ++
                --        "ms, timeout " ++ show timeout_ms

                LK.with var_lock $ do
                    cur_assign <- getA assign_var

                    if not $ null cur_assign then do
                        let unique_nodes = unique (map fst cur_assign)

                        -- progs <- forM unique_nodes (nodeTransState)
                        -- envMsg env $ "current progresses: " ++ show progs
                        envMsg env $ "load: " ++ show (length unique_nodes) ++
                                     " node(s) on " ++ show (length cur_assign) ++ " task(s)"

                        -- separate slow and good nodes
                        (slow, ok) <- flip sepWhenM cur_assign $ \(n, _) -> do
                            time <- nodeLastSeen n
                            return $ time < start_time

                        -- tLnM $ "separated " ++ show (length slow) ++ show (length ok)

                        let slow_nodes = unique $ map fst slow
                            ok_nodes = unique $ map fst ok

                        -- decrease/increase blacklist count
                        mapM_ nodeBlacklistDec ok_nodes
                        mapM_ nodeBlacklistInc slow_nodes

                        unless (null slow) $ do
                            -- we have slow nodes!
                            -- add them to the blacklist
                            new_blacklist <- appA (`SET.union` SET.fromList slow_nodes) blacklist_var

                            -- retry tasks of slow nodes
                            let retry_tasks = map snd slow

                            envMsg env $ "refetching on nodes " ++ show slow_nodes

                            new_assign <- reassign sched retry_tasks (SET.toList new_blacklist)

                            if null new_assign then do
                                -- failed to assign

                                -- blacklist full
                                -- try again with an empty blacklist
                                -- nodeMsg env node "failed to reassign tasks"

                                envWarn env "failed to reassign, elongate timeout"
                                -- elongate timeout
                                appA (*2) timeout_ms_var

                                assign <- failed_reassign sched retry_tasks

                                unless (null assign) $
                                    setA assign_var (ok ++ assign)    
                                -- else keep the original assignment
                            else
                                setA assign_var (ok ++ new_assign)
                    else do
                        -- empty cur_assign
                        -- kill the thread
                        envMsg env "self cancelling on empty assignment"
                        cancel sched

        -- watchExit mres =
        --     case mres of
        --         Right _ -> tLnM "scheduler exiting"
        --         Left err -> tLnM $ "scheduler exiting in error: " ++ show err

    tid <- envFork env THREAD_OTHER watch -- watchExit

    let sched = tmp_sched { sched_tid = tid }

    -- assign initital tasks
    init_assign sched >>= setA assign_var

    return sched
