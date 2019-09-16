module ComposeEffects
  ( tryComposeEffects
  ) where

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Random
import Control.Monad.IO.Class
import TryFused (runTeletypeIO, write)

-- random :: (Member RandomEffect.Random sig, Carrier sig m, SystemRandom.RandomGen k) =>  m k
-- random = send (RandomEffect.Random pure)

tryComposeEffects :: IO ()
tryComposeEffects = do

  runM . runTeletypeIO . evalRandomIO $ do
    n <- getRandomR (1000, 2000)
    liftIO $ print (n :: Integer)
    n <- getRandomR (1000, 2000)
    liftIO $ print (n :: Integer)

    liftIO $ putStrLn ","
    write "There."
    return ()


