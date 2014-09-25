{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
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
-- Assumes proper don ordering. Returns the missed and remaining dons.
pruneDons ∷ UnixTime → [Annotated Don] → ([Annotated Don], [Annotated Don])
pruneDons t = break ((t <) . _annotTime)

-- | Takes all the dons that fall within some specified length of time
-- in the future, given in milliseconds.
getDons ∷ UnixTime → Int32 → [Annotated Don] → [Annotated Don]
getDons t r = takeWhile inRange
  where
    inRange (Annot t' _) = addMs t (fromIntegral r) >= t'

-- | Adds set number of miliseconds to the 'UnixTime'.
addMs ∷ UnixTime → Int → UnixTime
addMs t i = addUnixDiffTime t $ microSecondsToUnixDiffTime (i * 1000)

toSS ∷ MonadIO m ⇒ (FilePath, TaikoData) → m SongState
toSS (_, d) = do
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
renderPos ∷ UnixTime → Annotated Don → Double
renderPos t (Annot t' _) = diffToMs $ t' `diffUnixTime` t

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

processKeys ∷ SongLoop ()
processKeys = do
  us ← use userSettings
  imgs ← use (resources . images)
  let onKey k = whenM (keyPress (us ^. k))
      k -!> (d, b) = do
        let k' = us ^. k
            bkd = screenState . blocking
        whenM (keyUp k') $ bkd %= filter ((/= k') . fst)
        whenM (keyDown k') $ do
          blockedKeys ← map fst <$> use (screenState . blocking)
          when (k' `notElem` blockedKeys) $ do
            bkd %= ((k', imgs ^. b):)
            screenState . waitingFor .= Just d

  onKey quitKey $ quit .= True
  outsideLeft -!> (BigBlue, outerLeftPressed)
  outsideRight -!> (BigBlue, outerRightPressed)
  insideLeft -!> (SmallRed, innerLeftPressed)
  insideRight -!> (SmallRed, innerRightPressed)

renderInfo ∷ [(Key, Bitmap)] → SongLoop ()
renderInfo bkd = do
  scr ← use (screenState . score)
  fnt ← use (resources . font)
  fps ← getFPS
  color red $ translate (V2 10 10) $ text fnt 10 (show fps)
  color yellow . translate (V2 30 30) . text fnt 10 $ show scr
  color yellow . translate (V2 30 45) . text fnt 10 $ show (map fst bkd)

prune ∷ UnixTime → SongLoop [Annotated Don]
prune ct = do
  ds ← use (screenState . dons)
  let (m, r) = pruneDons ct ds
  screenState . score . scoreMiss += length m
  screenState . dons .= r
  return r

songLoop ∷ SongLoop ()
songLoop = do
  innerLoop

-- | Adds one to the given score lens and clears the waiting state.
addScore ∷ Num a ⇒ Lens' Score a → SongLoop ()
addScore l = do
  screenState . score . l += 1
  screenState . waitingFor .= Nothing

(↠) :: SongLoop a → ([Annotated Don] → [Annotated Don]) → SongLoop ()
x ↠ f = x >> (screenState . dons %= f)

-- | Produces the horizontal offset of the goal's render position.
-- Effectively half the size of the goal bitmap for now.
goalOffset ∷ SongLoop Double
goalOffset = do
 g ← use (resources . images . goal)
 return . (/ 2) . fromIntegral . fst $ bitmapSize g

-- | Renders the given bitmap at the same place as the goal itself.
-- Useful for goal overlays and the goal itself.
renderAtGoal ∷ Bitmap → SongLoop ()
renderAtGoal b = do
  Box _ (V2 _ y) ← getBoundingBox
  widthOffset ← goalOffset
  translate (V2 widthOffset (y / 2)) (bitmap b)

renderElements ∷ UnixTime → SongLoop ()
renderElements ct = do
  bkd ← use (screenState . blocking)
  imgs ← use (resources . images)
  -- Score info &c
  renderInfo bkd

  -- Goal shape
  renderAtGoal (imgs ^. goal)

  -- Goal overlays
  mapM_ (renderAtGoal . snd) bkd

  -- Upcoming dons
  ds ← use (screenState . dons)
  Box _ (V2 _ y) ← getBoundingBox

  -- Render at goal + offset by time
  let rend d = translate (V2 (renderPos ct d) 0) $ renderAtGoal (getBmp imgs d)
  mapM_ rend $ getDons ct 5000 ds

innerLoop ∷ SongLoop ()
innerLoop = do
  processKeys
  ct ← liftIO getUnixTime
  remaining ← prune ct

  when (length remaining > 0) $ use (screenState . waitingFor) >>= \case
    Nothing → return ()
    Just d → case check ct (head remaining) d of
      Perfect → addScore scorePerfect ↠ drop 1
      Good    → addScore scoreGood    ↠ drop 1
      Bad     → addScore scoreBad     ↠ drop 1
      Wrong   → addScore scoreWrong   ↠ drop 1
      NOP     → addScore scoreCalmDown
      _       → return ()

  renderElements ct
  tick >> unlessM (use quit) innerLoop
