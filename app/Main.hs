module Main where

import Lib

-- main :: IO ()
-- main = do putStrLn "Hello, World!"
--           someFunc
--           return ()
import System.IO
import Data.IORef

incRef :: IORef Int -> IO ()
incRef var = do
    val <- readIORef var
    writeIORef var (val+1)

main :: IO ()
main = do
    (lq, wrapUpLogging) <- setupLog
    var <- newIORef 42
    incRef var 
    val <- readIORef var
    print val

    elf_group <- newGroup 3
    sequence_ [ elf lq elf_group n | n <- [1..10] ]

    rein_group <- newGroup 9
    sequence_ [ reindeer lq rein_group n | n <- [1..9] ]

    awhile (santa lq elf_group rein_group)

    testDelay lq

    wrapUpLogging
