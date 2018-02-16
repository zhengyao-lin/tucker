module Tucker.P2P.Scheduler where

import qualified Data.Set as SET

import Control.Monad
import Control.Concurrent
import Control.Concurrent.Thread.Delay
import qualified Control.Concurrent.Lock as LK

import Tucker.Atom
import Tucker.Util
import Tucker.P2P.Node

-- task scheduler
data Scheduler t =
    Scheduler {
        sched_tid  :: ThreadId,
        var_lock   :: LK.Lock,
        blacklist  :: Atom (SET.Set Node),
        cur_assign :: Atom [(Node, t)]
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
    sched_tid = tid
}) = killThread tid

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
             -> (Scheduler t -> [t] -> IO [(Node, t)])
             -> (Scheduler t -> [t] -> IO [(Node, t)])
             -> IO (Scheduler t)
newScheduler env timeout_s init_assign reassign failed_reassign = do
    let timeout_us = fi $ timeout_s * 1000 * 1000

    var_lock <- LK.new

    blacklist_var <- newA SET.empty
    assign_var <- newA []

    let tmp_sched = Scheduler {
            sched_tid = undefined,
            var_lock = var_lock,
            blacklist = blacklist_var,
            cur_assign = assign_var
        }

    tid <- forkIO $ do
        tid <- myThreadId
        let sched = tmp_sched { sched_tid = tid }

        forever $ do
            start_time <- unixTimestamp
            delay timeout_us

            LK.with var_lock $ do
                cur_assign <- getA assign_var

                progs <- forM cur_assign $ \(n, _) -> getA (cur_progress n)
                envMsg env $ "current progresses: " ++ show progs

                -- separate slow and good nodes
                (slow, ok) <- flip sepWhenM cur_assign $ \(n, _) -> do
                    time <- getA (last_seen n)
                    return $ time < start_time

                let slow_nodes = unique $ map fst slow
                    ok_nodes = unique $ map fst ok

                -- decrease/increase blacklist count
                mapM_ nodeBlacklistDec ok_nodes
                mapM_ nodeBlacklistInc slow_nodes

                if not $ null slow then do
                    -- we have slow nodes!
                    -- add them to the blacklist
                    appA (`SET.union` SET.fromList slow_nodes) blacklist_var

                    -- retry tasks of slow nodes
                    let retry_tasks = map snd slow

                    -- nodeMsg env node $ "refetching on nodes " ++ show retry_hashes

                    new_assign <- reassign sched retry_tasks

                    if null new_assign then do
                        -- failed to assign

                        -- blacklist full
                        -- try again with an empty blacklist
                        -- nodeMsg env node "failed to reassign tasks"

                        assign <- failed_reassign sched retry_tasks

                        if not $ null assign then
                            setA assign_var (ok ++ assign)
                        else
                            return ()
                            -- keep the original assignment
                    else
                        setA assign_var (ok ++ new_assign)
                else return ()

    let sched = tmp_sched { sched_tid = tid }

    -- assign initital tasks
    init_assign sched >>= setA assign_var

    return sched
