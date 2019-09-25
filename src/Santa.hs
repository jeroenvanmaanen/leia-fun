module Santa
    ( elf
    , reindeer
    , newGroup
    , santa
    , mainSanta
    , awhile
    , testDelay
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

data Gate = MkGate Int (TVar Int)

newGate :: Int -> STM Gate
newGate n = do
    tv <- newTVar 0
    return (MkGate n tv)

passGate :: (MonadIO m) => Gate -> m ()
passGate (MkGate n tv)
  = liftIO $ atomically (do n_left <- readTVar tv
                            check (n_left > 0)
                            writeTVar tv (n_left-1))

operateGate :: (MonadIO m) => Gate -> m ()
operateGate (MkGate n tv) = liftIO $ do
    atomically (writeTVar tv n)
    atomically (do n_left <- readTVar tv
                   check (n_left == 0))

data Group = MkGroup Int (TVar (Int, Gate, Gate))

newGroup   :: (MonadIO m) => Int -> m Group
newGroup n = liftIO $ atomically (do g1 <- newGate n; g2 <- newGate n
                                     tv <- newTVar (n, g1, g2)
                                     return (MkGroup n tv))

joinGroup  :: (MonadIO m) => Group -> m (Gate,Gate)
joinGroup (MkGroup n tv)
  = liftIO $ atomically (do (n_left, g1, g2) <- readTVar tv
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

meetInStudy :: (Member LogEffect sig, Carrier sig m, MonadIO m) => Int -> m ()
meetInStudy id = info $ "Elf " ++ show id ++ " meeting in the study"

deliverToys :: (Member LogEffect sig, Carrier sig m, MonadIO m) => Int -> m ()
deliverToys id = info $ "Reindeer " ++ show id ++ " delivering toys"

helper1 :: (Member LogEffect sig, Carrier sig m, MonadIO m) => Group -> m () -> m ()
helper1 group do_task = do
    (in_gate, out_gate) <- joinGroup group
    passGate in_gate
    randomDelay 1
    info "Doing task"
    do_task
    info "Task done"
    passGate out_gate

elf1, reindeer1 :: (Member LogEffect sig, Carrier sig m, MonadIO m) => Group -> Int -> m ()
elf1      gp id = helper1 gp (meetInStudy id)
reindeer1 gp id = helper1 gp (deliverToys id)

elf :: (Member LogEffect sig, Carrier sig m, MonadIO m) => Group -> Int -> m ThreadId
elf gp id = liftIO $ forkIO (runM . runLogEffect . awhile $ do randomDelay 10
                                                               elf1 gp id)

reindeer :: (Member LogEffect sig, Carrier sig m, MonadIO m) => Group -> Int -> m ThreadId
reindeer gp id = liftIO $ forkIO (runM . runLogEffect . awhile $ do randomDelay 10
                                                                    reindeer1 gp id)

santa :: (Member LogEffect sig, Carrier sig m, MonadIO m) => Group -> Group -> m ()
santa elf_gp rein_gp = do
    info ">>>"
    (task, (in_gate, out_gate)) <- liftIO $ atomically (orElse
                     (chooseGroup rein_gp "deliver toys")
                     (chooseGroup elf_gp "meet in my study"))
    info $ "Ho! Ho! Ho! let’s " ++ task
    operateGate in_gate
              -- Now the helpers do their task
    operateGate out_gate
    info "<<<"
  where
    chooseGroup :: Group -> String -> STM (String, (Gate,Gate))
    chooseGroup gp task = do gates <- awaitGroup gp
                             return (task, gates)

mainSanta :: (Member LogEffect sig, Carrier sig m, MonadIO m) => m ()
mainSanta = do
    elf_group <- newGroup 3
    sequence_ [ elf elf_group n | n <- [1..10] ]

    rein_group <- newGroup 9
    sequence_ [ reindeer rein_group n | n <- [1..9] ]

    awhile (santa elf_group rein_group)

awhile :: (MonadIO m) => m () -> m ()
-- Repeatedly perform the action
awhile act = forever' act 10
  where -- cheating here to make it stop eventually
    forever' :: (MonadIO m) => m () -> Int -> m ()
    forever' act 0 = return ()
    forever' act n = do
        act
        forever' act (n - 1)

randomDelay :: (Member LogEffect sig, Carrier sig m, MonadIO m) => Int -> m ()
-- Delay for a random time between 1 and 1,000,000 microseconds
randomDelay seconds = do
  waitTime <- liftIO $ getStdRandom (randomR (1, seconds * 1000000))
  info $ "Pause: " ++ show waitTime
  liftIO $ threadDelay waitTime
  info $ "Resume: " ++ show waitTime

randomOp :: (MonadIO m) => m String
randomOp = do
    n <- liftIO $ getStdRandom (randomR (0, 3))
    return (["+", "-", "*", "/"] !! n)

testDelay :: (Member LogEffect sig, Carrier sig m, MonadIO m) => m ()
testDelay = do
    info "All our operators are currently busy..."
    randomDelay 5
    op <- randomOp
    info $ "Operator (" ++ op ++ ") here, how may I help you?"
