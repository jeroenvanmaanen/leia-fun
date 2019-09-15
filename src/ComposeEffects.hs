{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ComposeEffects
  ( tryComposeEffects
  ) where

import Control.Effect
import Control.Effect.Carrier
import qualified Control.Effect.Random as RandomEffect
import Control.Monad.IO.Class
import qualified System.Random as SystemRandom
import TryFused (runTeletypeIO, write)

-- random :: (Member RandomEffect.Random sig, Carrier sig m, SystemRandom.RandomGen k) =>  m k
-- random = send (RandomEffect.Random pure)

tryComposeEffects :: IO ()
tryComposeEffects = do

  runM . runTeletypeIO . RandomEffect.evalRandomIO $ do
    g <- RandomEffect.getRandomR (10, 20)
    liftIO $ print (g ::Integer)

    liftIO $ putStrLn ","
    write "There."
    return ()


