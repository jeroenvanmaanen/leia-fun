module Main where

import Control.Concurrent
import Control.Effect
import Control.Monad
import Control.Monad.IO.Class
import LEIA.LEIA
import LEIA.Logging
import Santa
import Scratch
import TryFused
import ComposeEffects

main :: IO ()
main = do
  runM . runLogEffect . runTeletypeIO $ do
    info "bliep"
    tryFusedE
    testScratchE
    someFunc
    tryComposeEffects
    testDelay
    mainSanta
