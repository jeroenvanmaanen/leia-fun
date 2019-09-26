module ComposeEffects
  ( tryComposeEffects
  ) where

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Random
import Control.Monad
import Control.Monad.IO.Class
import LEIA.Logging
import TryFused (runTeletypeIO, write)

tryComposeEffects :: (MonadIO m) => m ()
tryComposeEffects = do

  runM . runLogEffect . runTeletypeIO . evalRandomIO $ do
    write "Hello, world!"
    n <- getRandomR (1000, 2000)
    info $ "n = " ++ (show (n :: Integer))
    n <- getRandomR (1000, 2000)
    info $ "n = " ++ (show (n :: Integer))

    info "The End."
