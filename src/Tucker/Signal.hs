-- utils for proc signal handling

module Tucker.Signal(noStop) where

import System.Posix.Signals

import Foreign.C.Error

import Tucker.Util

instance Show SignalSpecificInfo where
    show NoSignalSpecificInfo = "no specific info"
    show (SigChldInfo {
        siginfoPid = pid,
        siginfoUid = uid,
        siginfoStatus = status
    }) =
        "pid: " ++ show pid ++ ", uid: " ++ show uid ++ ", status: " ++ show status

noStop :: IO a -> IO a
noStop action = do
    installHandler sigINT handler (Just fullSignalSet)
    res <- action
    installHandler sigINT Default (Just fullSignalSet)
    return res
    where
        handler = CatchInfo $ \(SignalInfo {
            siginfoSignal = sig,
            siginfoError = Errno errno,
            siginfoSpecific = spec
        }) ->
            tLnM $ "no stop process recevied a signal: " ++ show sig ++
                   ", errno: " ++ show errno ++ ", specific info: " ++ show spec
