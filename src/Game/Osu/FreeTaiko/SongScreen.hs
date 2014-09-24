{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
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
import Data.Default
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
  return $ SS { _dons = ds
              , _elapsed = 0
              , _lastTick = ct
              , _waitingFor = Nothing
              , _score = def
              , _blocking = []
              }

  where
    toDon ∷ UnixTime → Int → Annotated Don
    toDon t offs = Annot (addMs t offs) SmallRed

diffToMs ∷ UnixDiffTime → Double
diffToMs (UnixDiffTime (CTime s) ms) =
  fromIntegral (s * 1000) + (fromIntegral ms / 1000)

-- | Give current time, a don and window width, produces the
-- horizontal position for the don to be rendered at.
renderPos ∷ UnixTime → Annotated Don → Double → Double
renderPos t (Annot t' _) w = diffToMs $ t' `diffUnixTime` t

getBmp ∷ Images → Annotated Don → Bitmap
getBmp i (Annot _ d) = i ^. case d of
  SmallBlue → smallBlue
  SmallRed → smallRed
  _ → smallBlue

check ∷ UnixTime → Annotated Don → Don → Hit
check ct (Annot t d) d' =
  let msc = diffToMs (t `diffUnixTime` ct) in case () of
    _ | msc <= 150 && d /= d' → Wrong
      | msc <= 50 && d == d' → Perfect
      | msc <= 100 → Good
      | msc <= 150 → Bad
      | otherwise → NOP

songLoop ∷ SongLoop ()
songLoop = do
  ct ← liftIO getUnixTime
  fnt ← use (resources . font)
  ds ← use (screenState . dons)
  q ← use quit
  Box _ (V2 x y) ← getBoundingBox
  us ← use userSettings

  let onKey k = whenM (keyPress (us ^. k))
      k -!> d = do
        let k' = us ^. k
            bkd = screenState . blocking
        whenM (keyUp k') $ bkd %= filter (/= k')
        whenM (keyDown k') $ do
          blockedKeys ← use (screenState . blocking)
          when (k' `notElem` blockedKeys) $ do
            bkd %= (k':)
            screenState . waitingFor .= Just d

  onKey quitKey $ quit .= True
  outsideLeft -!> BigBlue
  outsideRight -!> BigBlue
  insideLeft -!> SmallRed
  insideRight -!> SmallRed

  let pruned = pruneDons ct ds
      next = getDons ct (round x) pruned

  when (length pruned > 0) $ use (screenState . waitingFor) >>= \case
    Nothing → return ()
    Just d → case check ct (head pruned) d of
      Perfect → ssuc scorePerfect pruned
      Good → ssuc scoreGood pruned
      Bad → ssuc scoreBad pruned
      Wrong → ssuc scoreWrong pruned
      Miss → ssuc scoreMiss pruned
      NOP → ssucp scoreCalmDown pruned

  fps ← getFPS
  imgs ← use (resources . images)

  let middle = (y / 2)
  color (Color 255 0 0 255) $ translate (V2 10 10) $ text fnt 10 (show fps)
  -- color green . translate (V2 (x - 25) 20) . text fnt 20 . show $ length next
  scr ← use (screenState . score)
  bkd ← use (screenState . blocking)
  color yellow . translate (V2 30 30) . text fnt 10 $ show scr
  color yellow . translate (V2 30 45) . text fnt 10 $ show bkd
  let goalImg = imgs ^. goal
      hg = (/ 2) . fromIntegral . fst . bitmapSize $ imgs ^. goal
      rd d = translate (V2 (renderPos ct d x + hg) (y / 2))
               $ bitmap (getBmp imgs d)

  translate (V2 hg middle) $ bitmap (imgs ^. goal)
  mapM_ rd next

  tick >> unless q songLoop
  where
    scoreUp l = do
      screenState . score . l += 1
      screenState . waitingFor .= Nothing
    ssuc l p = do
      scoreUp l
      screenState . dons .= drop 1 p
    ssucp l p = do
      scoreUp l
      screenState . dons .= p
