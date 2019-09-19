module Main where

import Control.Concurrent
import Control.Effect
import Control.Monad.IO.Class
import LEIA.LEIA
import LEIA.Logging
import Santa
import Scratch
import TryFused
import ComposeEffects

main :: IO ()
main = do
  runM . runLogEffect $ do
    info "bliep"
    testScratchE
  withLog $ \lq -> do
    -- mainSanta lq
    testDelay lq
    someFunc lq
    tryFused lq
    tryComposeEffects
