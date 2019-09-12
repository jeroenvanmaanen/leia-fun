module Main where

import LEIA.LEIA
import LEIA.Logging
import Santa
import Scratch

main :: IO ()
main = withLog $ \lq -> do
    testScratch lq
    mainSanta lq
    testDelay lq
    someFunc lq
