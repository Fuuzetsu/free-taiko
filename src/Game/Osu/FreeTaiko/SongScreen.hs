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

-- | Adds set number of miliseconds to the 'UnixTime'.
addMs ∷ UnixTime → Int → UnixTime
addMs t i = addUnixDiffTime t $ microSecondsToUnixDiffTime (i * 1000)

toSS ∷ MonadIO m ⇒ (FilePath, TaikoData) → m SongState
toSS (p, d) = do
  ct ← liftIO getUnixTime
  let ds = [ toDon ct i | Spinner (_, _, i, _, _, _) ← _tdHitObjects d ]
  return $ SS { _dons = ds , _elapsed = 0 , _lastTick = ct }

  where
    toDon ∷ UnixTime → Int → Annotated Don
    toDon t offs = Annot (addMs t offs) SmallRed

-- | Give current time, a don and window width, produces the
-- horizontal position for the don to be rendered at.
renderPos ∷ UnixTime → Annotated Don → Double → (Double, Color)
renderPos t (Annot t' _) w =
  let UnixDiffTime (CTime s) ms = t' `diffUnixTime` t
      msc ∷ Double
      msc = fromIntegral (s * 1000) + (fromIntegral ms / 1000)

  in (msc {- / (w / 1000)-}, case () of _ | msc <= 200 → green
                                          | msc <= 500 → yellow
                                          | otherwise  → red)

getBmp ∷ Images → Annotated Don → Bitmap
getBmp i (Annot _ d) = i ^. case d of
  SmallBlue → smallBlue
  SmallRed → smallRed
  _ → smallBlue

songLoop ∷ SongLoop ()
songLoop = do
  ct ← liftIO getUnixTime
  fnt ← use (resources . font)
  ds ← use (screenState . dons)
  q ← use quit
  Box _ (V2 x y) ← getBoundingBox
  whenM (keyPress KeyEscape) $ quit .= True
  let pruned = pruneDons ct ds
      next = getDons ct (round x) pruned
  screenState . dons .= pruned
  fps ← getFPS
  imgs ← use (resources . images)
  color (Color 255 0 0 255) $ translate (V2 10 10) $ text fnt 10 (show fps)
  -- color green . translate (V2 (x - 25) 20) . text fnt 20 . show $ length next
  -- color yellow . translate (V2 0 (y - 15)) . text fnt 10 $ show next
  let rd d = let (xp, c) = renderPos ct d x
             in translate (V2 xp (y / 2)) $ bitmap (getBmp imgs d)
  mapM_ rd next

  tick >> unless q songLoop
