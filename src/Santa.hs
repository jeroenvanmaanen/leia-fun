module Santa
    ( elf
    , reindeer
    , newGroup
    , santa
    , mainSanta
    , awhile
    , testDelayE
    ) where

-- https://www.schoolofhaskell.com/school/advanced-haskell/beautiful-concurrency/4-the-santa-claus-problem
-- https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/mark.pdf
-- https://simonmar.github.io/bib/papers/async.pdf

import Control.Concurrent.STM
import Control.Concurrent
import Control.Effect
import Control.Effect.Carrier
import Control.Monad.IO.Class
import LEIA.Logging
import System.Random
import qualified System.Log.Caster as Log

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

meetInStudy :: Log.LogQueue -> Int -> IO ()
meetInStudy lq id = Log.info lq $ "Elf " ++ show id ++ " meeting in the study"

deliverToys :: Log.LogQueue -> Int -> IO ()
deliverToys lq id = Log.info lq $ "Reindeer " ++ show id ++ " delivering toys"

helper1 :: Log.LogQueue -> Group -> IO () -> IO ()
helper1 lq group do_task = do
    (in_gate, out_gate) <- joinGroup group
    passGate in_gate
    randomDelay lq 1
    Log.info lq "Doing task"
    do_task
    Log.info lq "Task done"
    passGate out_gate

elf1, reindeer1 :: Log.LogQueue -> Group -> Int -> IO ()
elf1      lq gp id = helper1 lq gp (meetInStudy lq id)
reindeer1 lq gp id = helper1 lq gp (deliverToys lq id)

elf :: Log.LogQueue -> Group -> Int -> IO ThreadId
elf lq gp id = forkIO (awhile (do randomDelay lq 10
                                  elf1 lq gp id))

reindeer :: Log.LogQueue -> Group -> Int -> IO ThreadId
reindeer lq gp id = forkIO (awhile (do randomDelay lq 10
                                       reindeer1 lq gp id))

santa :: Log.LogQueue -> Group -> Group -> IO ()
santa lq elf_gp rein_gp = do
    Log.info lq  ">>>"
    (task, (in_gate, out_gate)) <- atomically (orElse
                     (chooseGroup rein_gp "deliver toys")
                     (chooseGroup elf_gp "meet in my study"))
    Log.info lq $ "Ho! Ho! Ho! let’s " ++ task
    operateGate in_gate
              -- Now the helpers do their task
    operateGate out_gate
    Log.info lq  "<<<"
  where
    chooseGroup :: Group -> String -> STM (String, (Gate,Gate))
    chooseGroup gp task = do gates <- awaitGroup gp
                             return (task, gates)

mainSanta :: Log.LogQueue -> IO ()
mainSanta lq = do
    elf_group <- newGroup 3
    sequence_ [ elf lq elf_group n | n <- [1..10] ]

    rein_group <- newGroup 9
    sequence_ [ reindeer lq rein_group n | n <- [1..9] ]

    awhile (santa lq elf_group rein_group)

awhile :: IO () -> IO ()
-- Repeatedly perform the action
awhile act = forever' act 10
  where -- cheating here to make it stop eventually
    forever' :: IO () -> Int -> IO ()
    forever' act 0 = return ()
    forever' act n = do
        act
        forever' act (n - 1)

randomDelay :: Log.LogQueue -> Int -> IO ()
-- Delay for a random time between 1 and 1,000,000 microseconds
randomDelay lq seconds = do
  waitTime <- getStdRandom (randomR (1, seconds * 1000000))
--  Log.info lq $ "Pause: " ++ show waitTime
  threadDelay waitTime
--  Log.info lq $ "Resume: " ++ show waitTime

randomDelayE :: (Member LogEffect sig, Carrier sig m, MonadIO m) => Int -> m ()
-- Delay for a random time between 1 and 1,000,000 microseconds
randomDelayE seconds = do
  waitTime <- liftIO $ getStdRandom (randomR (1, seconds * 1000000))
  info $ "Pause: " ++ show waitTime
  liftIO $ threadDelay waitTime
  info $ "Resume: " ++ show waitTime

randomOp :: (MonadIO m) => m String
randomOp = do
    n <- liftIO $ getStdRandom (randomR (0, 3))
    return (["+", "-", "*", "/"] !! n)

testDelayE :: (Member LogEffect sig, Carrier sig m, MonadIO m) => m ()
testDelayE = do
    info "All our operators are currently busy..."
    randomDelayE 5
    op <- randomOp
    info $ "Operator (" ++ op ++ ") here, how may I help you?"
