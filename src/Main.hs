{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Main
-- License     :  GPL-3
-- Copyright   :  © Mateusz Kowalczyk, 2014
-- Maintainer  :  fuuzetsu@fuuzetsu.co.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Main entry point to free-taiko

module Main where

import qualified Data.List.PointedList as PL
import qualified Data.List.PointedList.Circular as C
import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Default
import           Data.Monoid
import qualified Data.Text as T
import           FreeGame
import           Game.Osu.FreeTaiko.Menu
import           Game.Osu.FreeTaiko.SongScreen
import           Game.Osu.FreeTaiko.Types
import           Game.Osu.OszLoader.Types
import           System.Exit
import           System.FilePath ((</>))

run ∷ Game a → IO (Maybe a)
run = runGame (def ^. windowMode) (def ^. resolution . unR)

dir ∷ FilePath
dir = "/home/shana/oszs"

fontdir ∷ FilePath
fontdir = "/home/shana/programming/free-taiko/data/fonts/VL-PGothic-Regular.ttf"

imgdir ∷ FilePath
imgdir = "/home/shana/programming/free-taiko/data/images"

loadImages ∷ MonadIO m ⇒ m Images
loadImages = do
  sb ← readBitmap $ imgdir </> "small_blue.png"
  sr ← readBitmap $ imgdir </> "small_red.png"
  bb ← readBitmap $ imgdir </> "big_blue.png"
  br ← readBitmap $ imgdir </> "big_red.png"
  gl ← readBitmap $ imgdir </> "goal.png"
  oR ← readBitmap $ imgdir </> "outer_right_pressed.png"
  ol ← readBitmap $ imgdir </> "outer_left_pressed.png"
  ir ← readBitmap $ imgdir </> "inner_right_pressed.png"
  il ← readBitmap $ imgdir </> "inner_left_pressed.png"
  bg ← readBitmap $ imgdir </> "bg_1080p.png"
  bt ← readBitmap $ imgdir </> "belt.png"
  dr ← readBitmap $ imgdir </> "drum.png"
  hgl ← readBitmap $ imgdir </> "hit_greatL.png"
  hgs ← readBitmap $ imgdir </> "hit_greatS.png"
  hgl' ← readBitmap $ imgdir </> "hit_goodL.png"
  hgs' ← readBitmap $ imgdir </> "hit_goodS.png"
  hm ← readBitmap $ imgdir </> "hit_miss.png"
  let mkNum x = readBitmap $ imgdir </> "default-" ++ show x ++ ".png"
  nums ← mapM mkNum [0 .. 9 ∷ Integer]
  return $ Images { _smallBlue = sb
                  , _smallRed = sr
                  , _bigBlue = bb
                  , _bigRed = br
                  , _goal = gl
                  , _innerRightPressed = ir
                  , _innerLeftPressed = il
                  , _outerRightPressed = oR
                  , _outerLeftPressed = ol
                  , _bg1080p = bg
                  , _belt = bt
                  , _drum = dr
                  , _hitGreatL = hgl
                  , _hitGreatS = hgs
                  , _hitGoodL = hgl'
                  , _hitGoodS = hgs'
                  , _hitMiss = hm
                  , _numbers = nums
                  }

loadRes ∷ MonadIO m ⇒ m Resources
loadRes = do
  fnt ← loadFont fontdir
  img ← loadImages
  return $ Res { _font = fnt
               , _images = img }

renderResult ∷ (Picture2D m, FromFinalizer m, MonadIO m) ⇒ Double -- ^ font size
             → Double -- ^ spacing between each line
             → Font -- ^ Font to use
             → PL.PointedList (FilePath, Either T.Text TaikoData)
             → m ()
renderResult fs spacing fnt (PL.PointedList b c a) =
  zipWithM_ offset positions (reverse (map rend b)
                              ++ [rendFocus c]
                              ++ map rend a)
  where
    positions ∷ [Double]
    positions = map (\x → x * fs + spacing) [2 .. ]

    offset ∷ (MonadIO m, Affine m) ⇒ Double → m a → m a
    offset = translate . V2 10

    rend ∷ (FromFinalizer m, Picture2D m, MonadIO m)
         ⇒ (FilePath, Either T.Text TaikoData)
         → m ()
    rend (p, Left _) = color red $ text fnt fs p
    rend (_, Right x) = color green $ text fnt fs (mkTitle x)

    rendFocus ∷ (FromFinalizer m, Picture2D m, MonadIO m)
              ⇒ (FilePath, Either T.Text TaikoData)
              → m ()
    rendFocus (p, Left _) = color yellow $ text fnt fs p
    rendFocus (_, Right x) = color yellow $ text fnt fs (mkTitle x)

    mkTitle ∷ TaikoData → String
    mkTitle x = let x' = x ^. tdMetadata in case _titleUnicode x' of
      Nothing → (T.unpack . _title $ x') <> creator
      Just t → T.unpack t <> creator
      where
        creator = " -- " <> (T.unpack . _creator $ x ^. tdMetadata)

main ∷ IO ()
main = void . run $ do
  setFPS 60
  setTitle "free-taiko"
  clearColor $ Color 0 0 0 0
  runMenu dir >>= \case
    Nothing → liftIO $ print "No songs"
    Just m → do
      ss ← loadRes >>= return . mkMenu m
      s ← evalStateT menuLoop ss
      evalStateT songLoop (ss & screenState .~ s)
  where
    menuLoop ∷ MenuLoop SongState
    menuLoop = do
      bmaps ← use (screenState . maps)
      uset ← use userSettings

      whenM (keyPress $ uset ^. quitKey) $ quit .= True
      whenM (keyPress KeyUp) $ screenState . maps %= C.previous
      whenM (keyPress KeyDown) $ screenState . maps %= C.next
      whenM (keyPress KeyEnter) $ case PL._focus bmaps of
        (_, Left _) → return ()
        (p, Right x) → screenState . picked .= Just (p, x)

      use (screenState . picked) >>= \case
        Just s → toSS s >>= return
        Nothing → do
          fnt ← use (resources . font)
          renderResult 15 2 fnt bmaps
          fps ← getFPS
          color (Color 255 0 0 255) $ translate (V2 5 5) $ text fnt 5 (show fps)
          q ← use quit
          tick >> if q then liftIO exitSuccess else menuLoop
