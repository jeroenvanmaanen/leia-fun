module LEIA.Logging
    ( withLog
    ) where

import Control.Concurrent
import System.Log.Caster

setupLog :: IO (LogQueue, IO ())
setupLog = do
    chan <- newLogChan
    lq <- newLogQueue

    relayThreadId <- forkIO $ do
          relayLog chan LogInfo stdoutListener
          putStrLn "End relayLog"

    broadcastThreadId <- forkIO $ do
          broadcastLog lq chan
          putStrLn "End broadcastLog"

    info lq "----------"
    return (lq, threadDelay 100000)

withLog :: (LogQueue -> IO a) -> IO a
withLog action = do
    (lq, finalize) <- setupLog
    result <- action lq
    finalize
    return result
