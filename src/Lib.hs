module Lib
    ( someFunc
    , elf
    , reindeer
    , newGroup
    , santa
    , awhile
    , testDelay
    , setupLog
    ) where

import Control.Concurrent.STM
import Control.Concurrent
import System.Random
import System.Log.Caster

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Gate = MkGate Int (TVar Int)

newGate :: Int -> STM Gate
newGate n = do
    tv <- newTVar 0
    return (MkGate n tv)

passGate :: Gate -> IO ()
passGate (MkGate n tv)
  = atomically (do n_left <- readTVar tv
                   check (n_left > 0)
                   writeTVar tv (n_left-1))

operateGate :: Gate -> IO ()
operateGate (MkGate n tv) = do
    atomically (writeTVar tv n)
    atomically (do n_left <- readTVar tv
                   check (n_left == 0))

data Group = MkGroup Int (TVar (Int, Gate, Gate))

newGroup   :: Int -> IO Group
newGroup n = atomically (do g1 <- newGate n; g2 <- newGate n
                            tv <- newTVar (n, g1, g2)
                            return (MkGroup n tv))

joinGroup  :: Group -> IO (Gate,Gate)
joinGroup (MkGroup n tv)
  = atomically (do (n_left, g1, g2) <- readTVar tv
                   check (n_left > 0)
                   writeTVar tv (n_left-1, g1, g2)
                   return (g1,g2))

awaitGroup :: Group -> STM (Gate,Gate)
awaitGroup (MkGroup n tv) = do
    (n_left, g1, g2) <- readTVar tv
    check (n_left == 0)
    new_g1 <- newGate n; new_g2 <- newGate n
    writeTVar tv (n,new_g1,new_g2)
    return (g1,g2)

meetInStudy :: LogQueue -> Int -> IO ()
meetInStudy lq id = info lq $ "Elf " ++ show id ++ " meeting in the study"

deliverToys :: LogQueue -> Int -> IO ()
deliverToys lq id = info lq $ "Reindeer " ++ show id ++ " delivering toys"

helper1 :: LogQueue -> Group -> IO () -> IO ()
helper1 lq group do_task = do
    (in_gate, out_gate) <- joinGroup group
    passGate in_gate
    randomDelay lq 1
    info lq "Doing task"
    do_task
    info lq "Task done"
    passGate out_gate

elf1, reindeer1 :: LogQueue -> Group -> Int -> IO ()
elf1      lq gp id = helper1 lq gp (meetInStudy lq id)
reindeer1 lq gp id = helper1 lq gp (deliverToys lq id)

elf :: LogQueue -> Group -> Int -> IO ThreadId
elf lq gp id = forkIO (awhile (do randomDelay lq 10
                                  elf1 lq gp id))

reindeer :: LogQueue -> Group -> Int -> IO ThreadId
reindeer lq gp id = forkIO (awhile (do randomDelay lq 10
                                       reindeer1 lq gp id))

santa :: LogQueue -> Group -> Group -> IO ()
santa lq elf_gp rein_gp = do
    info lq  ">>>"
    (task, (in_gate, out_gate)) <- atomically (orElse
                     (chooseGroup rein_gp "deliver toys")
                     (chooseGroup elf_gp "meet in my study"))
    info lq $ "Ho! Ho! Ho! let’s " ++ task
    operateGate in_gate
              -- Now the helpers do their task
    operateGate out_gate
    info lq  "<<<"
  where
    chooseGroup :: Group -> String -> STM (String, (Gate,Gate))
    chooseGroup gp task = do gates <- awaitGroup gp
                             return (task, gates)

awhile :: IO () -> IO ()
-- Repeatedly perform the action
awhile act = forever' act 10
  where -- cheating here to make it stop eventually
    forever' :: IO () -> Int -> IO ()
    forever' act 0 = return ()
    forever' act n = do
        act
        forever' act (n - 1)

randomDelay :: LogQueue -> Int -> IO ()
-- Delay for a random time between 1 and 1,000,000 microseconds
randomDelay lq seconds = do
  waitTime <- getStdRandom (randomR (1, seconds * 1000000))
--  info lq $ "Pause: " ++ show waitTime
  threadDelay waitTime
--  info lq $ "Resume: " ++ show waitTime

randomOp :: IO String
randomOp = do
    n <- getStdRandom (randomR (0, 3))
    return (["+", "-", "*", "/"] !! n)

testDelay :: LogQueue -> IO ()
testDelay lq = do
    info lq "All our operators are currently busy..."
    randomDelay lq 5
    op <- randomOp
    info lq ("Operator (" ++ op ++ ") here, how may I help you?")

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
