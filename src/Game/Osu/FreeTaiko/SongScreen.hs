{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Game.Osu.FreeTaiko.SongScreen
-- License     :  GPL-3
-- Copyright   :  © Mateusz Kowalczyk, 2014
-- Maintainer  :  fuuzetsu@fuuzetsu.co.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- The main play area

module Game.Osu.FreeTaiko.SongScreen where

import Control.Monad.State.Strict
import Control.Lens
import Data.UnixTime
import Foreign.C.Types
import Game.Osu.FreeTaiko.Types
import FreeGame
import Data.Int
import Game.Osu.OszLoader.Types

-- | Cuts off all the dons that should have played in the past.
-- Assumes proper don ordering.
pruneDons ∷ UnixTime → [Annotated Don] → [Annotated Don]
pruneDons t = dropWhile ((t >) . _annotTime)

-- | Takes all the dons that fall within some specified length of time
-- in the future, given in milliseconds.
getDons ∷ UnixTime → Int32 → [Annotated Don] → [Annotated Don]
getDons t r = takeWhile inRange
  where
    diftime = UnixDiffTime 0 (r * 1000)
    inRange (Annot t' _) = addMs t (fromIntegral r) >= t'

-- | Adds set number of miliseconds to the 'UnixTime' works around the
-- questionable default behaviour at
-- https://github.com/kazu-yamamoto/unix-time/issues/27
addMs ∷ UnixTime → Int → UnixTime
addMs t i = addUnixDiffTime t (UnixDiffTime secs mcs)
  where
    secs :: CTime
    secs = fromIntegral $ i `div` 1000

    mcs :: Int32
    mcs = fromIntegral $ (i `mod` 1000) * 1000

toSS ∷ MonadIO m ⇒ (FilePath, TaikoData) → m SongState
toSS (p, d) = do
  ct ← liftIO getUnixTime
  let ds = [ toDon ct i | Spinner (_, _, i, _, _, _) ← _tdHitObjects d ]
  return $ SS { _dons = ds , _elapsed = 0 , _lastTick = ct }

  where
    toDon ∷ UnixTime → Int → Annotated Don
    toDon t offs = Annot (addMs t offs) SmallRed

songLoop ∷ SongLoop ()
songLoop = do
  ct ← liftIO getUnixTime
  fnt ← use (resources . font)
  ds ← use (screenState . dons)
  q ← use quit
  whenM (keyPress KeyEscape) $ quit .= True
  let pruned = pruneDons ct ds
      next = getDons ct 1000 pruned
  screenState . dons .= pruned
  fps ← getFPS
  color (Color 255 0 0 255) $ translate (V2 10 10) $ text fnt 10 (show fps)
  color green . translate (V2 200 200) . text fnt 20 . show $ length next
  color yellow . translate (V2 0 250) . text fnt 10 $ show next
  tick >> unless q songLoop
