-- mining utils

module Tucker.P2P.Mining where

import Data.Word

import Control.Monad
import Control.Concurrent

import Debug.Trace

import Tucker.Msg
import Tucker.Enc
import Tucker.Auth
import Tucker.Util
import Tucker.Atom

-- split the nonce guessing job to n different jobs
splitJob :: Integral t => t -> [[Word32]]
splitJob n' =
    [ [ per_job * x .. per_job * (x + 1) ] | x <- [ 0 .. n - 2 ] ] ++
    [ [ per_job * (n - 1) .. ] ]
    where n = fi n'
          per_job = maxBound `div` n

-- a naive mining function
-- append a 32-bit nonce to dat until its hash is lower than the target
-- and return the nonce
doMine :: ByteString -> Hash256 -> IO Word32
doMine dat target = return result
    where
        Just result = first check [ 0 .. maxBound ]
        check nonce =
            trace (show (fi nonce / fi (maxBound :: Word32) * 100.0) ++ "%") $
            stdHash256 (dat <> encodeLE nonce) <= target

-- run doMine in parallel
doMineParallel :: ByteString -> Hash256 -> (Word32 -> IO ()) -> IO ()
doMineParallel dat target callback = do
    cap <- getNumCapabilities
    let jobs = splitJob cap

    threads_var <- newA []

    let check nonce =
            let hash = stdHash256 (dat <> encodeLE nonce) in
            if nonce `mod` 4192 == 0 then
                t (show nonce ++ "(" ++ take 8 (show hash) ++ ")") $ hash <= target
            else
                hash <= target
            -- trace (show (fi nonce / fi (maxBound :: Word32) * 100.0) ++ "%") $

        gen job =
            case first check job of
                Just nonce -> do
                    callback nonce
                    getA threads_var >>= killAllThreads

                Nothing -> return () -- not found

    threads <- mapM forkIO (map gen jobs)
    setA threads_var threads

    return ()

doMineBlock :: Block -> IO Block
doMineBlock block = do
    let fixed = blockFixedHeader block
    
    -- nonce <- doMine fixed (hash_target block)

    begin <- msCPUTime

    doMineParallel fixed (hash_target block) $ \nonce -> do
        let nblock = updateBlockHashes $ block {
                    nonce = nonce
                }

        end <- msCPUTime

        tLnM ""
        tLnM ("time spent: " ++ show ((end - begin) `div` 1000) ++ "s")
        tLnM (show nonce)
        tLnM (show (encodeLE nblock))

    forever yield

    return undefined

    -- return $ updateBlockHashes $ block {
    --     nonce = nonce
    -- }
