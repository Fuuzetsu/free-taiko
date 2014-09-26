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
  let ds = [ toDon ct i t | Spinner (_, _, i, _, t, _) ← _tdHitObjects d ]
  return $ SS { _dons = ds
              , _elapsed = 0
              , _lastTick = ct
              , _waitingFor = Nothing
              , _score = def
              , _blocking = def
              , _flyingOff = def
              , _songCombo = def
              , _taikoData = d
              , _goalEffects = def
              }

  where
    toDon ∷ UnixTime → Int → Int → Annotated Don
    toDon t offs t' = Annot (addMs t offs) $ case t' of
      0  → SmallRed
      4  → BigRed
      8  → SmallBlue
      12 → BigBlue
      _  → SmallRed

-- | Calculate the base hit value for the song
baseHit ∷ SongState → Int
baseHit _ = 80

diffToMs ∷ UnixDiffTime → Double
diffToMs (UnixDiffTime (CTime s) ms) =
  fromIntegral (s * 1000) + (fromIntegral ms / 1000)

-- | Give current time, a don and window width, produces the
-- horizontal position for the don to be rendered at.
renderPos ∷ UnixTime → Annotated Don → Double
renderPos t (Annot t' _) = diffToMs $ t' `diffUnixTime` t

getBmp ∷ Images → Don → Bitmap
getBmp i d = i ^. case d of
  SmallBlue → smallBlue
  SmallRed → smallRed
  BigBlue → bigBlue
  BigRed → bigRed

getHitBmp ∷ Images → Don → Hit → Bitmap
getHitBmp i d h = i ^. case (d, h) of
  (SmallBlue, Perfect) → hitGreatS
  (SmallRed, Perfect) → hitGreatS
  (BigBlue, Perfect) → hitGreatL
  (BigRed, Perfect) → hitGreatL
  (SmallBlue, Good) → hitGoodS
  (SmallRed, Good) → hitGoodS
  (BigBlue, Good) → hitGoodL
  (BigRed, Good) → hitGoodL
  (_, _) → hitMiss

check ∷ UnixTime → Annotated Don → Don → Difficulty → Hit
check ct (Annot t d) d' (Difficulty { _overallDifficulty = od }) =
  let msc = diffToMs (t `diffUnixTime` ct) in case () of
    _ | msc <= lu50 && d /= d' → Wrong
      | msc <= lu300 && d == d' → Perfect
      | msc <= lu100 → Good
      | msc <= lu50 → Bad
      | otherwise → NOP
  where
    -- Values for no mods, http://w.ppy.sh/4/46/ODChart.png
    lu300 = fromIntegral $ 78 - 6 * od
    lu100 = fromIntegral $ 138 - 8 * od
    lu50  = fromIntegral $ 198 - 10 * od

calcAccuracy ∷ Score → Double
calcAccuracy (Score p g b w m _ _) =
  let n = fromIntegral $ p + g + b + w + m
  in if n == 0
     then n
     else (fromIntegral p / n + fromIntegral g * 0.5 / n) * 100

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
  outsideLeft -!> (SmallBlue, outerLeftPressed)
  outsideRight -!> (BigBlue, outerRightPressed)
  insideLeft -!> (SmallRed, innerLeftPressed)
  insideRight -!> (SmallRed, innerRightPressed)

renderInfo ∷ [(Key, Bitmap)] → SongLoop ()
renderInfo bkd = do
  scr ← use (screenState . score)
  fnt ← use (resources . font)
  fps ← getFPS
  cmb ← use (screenState . songCombo)
  acc ← calcAccuracy <$> use (screenState . score)
  pts ← use (screenState . score . scorePoints)
  color red $ translate (V2 10 10) $ text fnt 10 (show fps)
  color yellow . translate (V2 30 30) . text fnt 10 $ show scr
  color yellow . translate (V2 30 45) . text fnt 10 $ show (map fst bkd)
  color green  . translate (V2 30 60) . text fnt 10 $ show cmb
  color green  . translate (V2 30 75) . text fnt 10 $ show acc ++ "%"
  color green  . translate (V2 30 90) . text fnt 10 $ show pts

prune ∷ UnixTime → SongLoop [Annotated Don]
prune ct = do
  ds ← use (screenState . dons)
  let (m, r) = pruneDons ct ds
  screenState . score . scoreMiss += length m
  screenState . dons .= r
  unless (null m) (runMiss ct)
  return r

songLoop ∷ SongLoop ()
songLoop = do
  innerLoop

-- | Adds one to the given score lens and clears the waiting state.
addScore ∷ Num a ⇒ Lens' Score a → SongLoop ()
addScore l = do
  screenState . score . l += 1
  screenState . waitingFor .= Nothing

-- | Drops one don and puts it into the list of dons that are ‘flying
-- off’ from the goal towards the top of the screen.
fly :: UnixTime → SongLoop ()
fly ct = use (screenState . dons) >>= \case
  [] → return ()
  (Annot _ x) : xs → do
    screenState . dons .= xs
    screenState . flyingOff %= (Annot ct x:)

-- As 'goalOffset' but for drum
drumOffset ∷ SongLoop Double
drumOffset = do
  d ← use (resources . images . drum)
  let (dw, _) = bitmapSize d
  return $ fromIntegral dw / 2

-- | Produces the horizontal offset of the goal's render position.
-- Effectively half the size of the goal bitmap for now.
goalOffset ∷ SongLoop Double
goalOffset = do
 g ← use (resources . images . goal)
 b ← use (resources . images . belt)
 let (bw, _) = bitmapSize b
     (gw, _) = bitmapSize g
     -- render goal this much into the belt
     r = 0.1479 -- 0.2

 return $ fromIntegral bw * r + fromIntegral gw / 2

renderOver :: SongLoop Double -> Bitmap -> SongLoop ()
renderOver l b = do
  gh ← goalHeight
  wo ← l
  translate (V2 wo gh) (bitmap b)

-- | Renders the given bitmap at the same place as the goal itself.
-- Useful for goal overlays and the goal itself.
renderAtGoal ∷ Bitmap → SongLoop ()
renderAtGoal = renderOver goalOffset

renderAtDrum ∷ Bitmap → SongLoop ()
renderAtDrum = renderOver drumOffset

-- | Time for the don to spend in flying stage
flyingTime ∷ Double
flyingTime = 500

beltOffset ∷ Double
beltOffset = 0.41

pruneFlying ∷ UnixTime → SongLoop ()
pruneFlying ct = do
  let ft = round flyingTime
  screenState . flyingOff %= filter ((ct <) . flip addMs ft . _annotTime)

pruneAnimations ∷ UnixTime → SongLoop ()
pruneAnimations ct = screenState . goalEffects %= filter ((ct <) . _endTime)

renderFlying ∷ UnixTime → Annotated Don → SongLoop ()
renderFlying ct (Annot t don) = do
  bmp ← flip getBmp don <$> use (resources . images)
  let diff = diffToMs $ ct `diffUnixTime` t
  if diff > flyingTime
    then return ()
    else do
    gh ← goalHeight
    let px = 200 / flyingTime * diff
        py = negate $ gh / flyingTime * diff
        a  = 1 - (diff / flyingTime)
    color (Color 1 1 1 (realToFrac a))
      . translate (V2 px py) . scale (V2 a a) $ renderAtGoal bmp

sucCombo ∷ SongLoop ()
sucCombo = do
  Combo c m ← use (screenState . songCombo)
  let nc = succ c
  screenState . songCombo .= Combo nc (max nc m)

resetCombo ∷ SongLoop ()
resetCombo = screenState . songCombo . currentCombo .= 0

-- | Increases the score based on the hit scored
incScore ∷ Hit → SongLoop ()
incScore h = do
  ss ← use screenState
  c' ← use (screenState . songCombo . currentCombo)
  let c ∷ Double
      c = fromIntegral c' / 10
      m = modifier h $ 300 + floor c * baseHit ss
      ma = modifier h $ 300 + 10 * baseHit ss

  screenState . score . scorePoints += min m ma
  where
    modifier Perfect x = x
    modifier Good x = x `div` 2
    modifier _ _ = 0

goalHeight ∷ SongLoop Double
goalHeight = do
  Box _ (V2 _ sy) ← getBoundingBox
  return $ sy * beltOffset

renderElements ∷ UnixTime → SongLoop ()
renderElements ct = do
  bkd ← use (screenState . blocking)
  imgs ← use (resources . images)
  let center b = bitmapSize b & both %~ ((/ 2) . fromIntegral)

  -- Background
  let (w, h) = center (imgs ^. bg1080p)
  translate (V2 w h) (bitmap (imgs ^. bg1080p))

  -- Drum, goes under the belt
  renderAtDrum (imgs ^. drum)

  -- Drum overlays
  mapM_ (renderAtDrum . snd) bkd

  -- Belt
  let (bw, b) = (fst $ center b, imgs ^. belt)
  gh ← goalHeight
  translate (V2 bw gh) (bitmap b)

  -- Score info &c
  renderInfo bkd

  -- Goal shape
  renderAtGoal (imgs ^. goal)

  -- Goal animations
  efs ← use (screenState . goalEffects)
  go ← goalOffset
  mapM_ (translate (V2 go gh) . flip _animate ct) efs

  -- Upcoming dons, render at goal + offset by time
  ds ← use (screenState . dons)
  let rend d = translate (V2 (renderPos ct d) 0)
               $ renderAtGoal (getBmp imgs $ _unAnnot d)
  mapM_ rend $ getDons ct 5000 ds

  -- Dons that are flying off the screen
  fl ← use (screenState . flyingOff)
  mapM_ (renderFlying ct) fl

innerLoop ∷ SongLoop ()
innerLoop = do
  processKeys
  ct ← liftIO getUnixTime
  remaining ← prune ct
  pruneFlying ct
  pruneAnimations ct

  dif ← use (screenState . taikoData . tdDifficulty)
  when (length remaining > 0) $ use (screenState . waitingFor) >>= \case
    Nothing → return ()
    Just d → case check ct (head remaining) d dif of
      Perfect → addScore scorePerfect  >> success ct Perfect d
      Good    → addScore scoreGood     >> success ct Good d
      Bad     → addScore scoreBad      >> resetCombo >> runMiss ct
      Wrong   → addScore scoreWrong    >> resetCombo >> runMiss ct
      NOP     → addScore scoreCalmDown
      _       → return ()

  renderElements ct

  q ← use quit
  tick >> unless (q || null remaining) innerLoop
  where
    success ∷ UnixTime → Hit → Don → SongLoop ()
    success t h d = do
      imgs ← use (resources . images)
      screenState . goalEffects %= (pulse t (getHitBmp imgs d h):)
      fly t >> incScore h >> sucCombo

runMiss ∷ UnixTime → SongLoop ()
runMiss t = use (resources . images) >>= \i → pulseBmp t (i ^. hitMiss)

pulseBmp ∷ UnixTime → Bitmap → SongLoop ()
pulseBmp t b = screenState . goalEffects %= (pulse t b:)

 -- Creates a ‘pulse’ animation over 200ms, used for hit effects.
pulse ∷ UnixTime → Bitmap → Animation SongLoop ()
pulse t b = let dur = 300
            in A { _endTime = addMs t dur
                 , _animate = \ct → case pulseF dur ct t of
                   (o, sc) → color (Color 1 1 1 (realToFrac o))
                             . scale (V2 sc sc)
                             $ (bitmap b)
                 }
  where
    -- Taking two times, returns the opacity and scaling needed for
    -- the ‘pulse’ effect.
    pulseF ∷ Int → UnixTime → UnixTime → (Double, Double)
    pulseF dur c st =
      let diff = diffToMs $ c `diffUnixTime` st
          dh = fromIntegral $ dur `div` 3
          minsc = 0.8
      in case () of
        _ | diff > fromIntegral dur → (0, 1)
          | diff < dh  → let r = 1 / (dh / diff) in (r, max r minsc)
          | diff >= dh → let r = 2 - (1 / (dh / diff)) in (r, max r minsc)
          | otherwise  → (0, 1)
