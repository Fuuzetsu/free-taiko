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
import           Game.Osu.FreeTaiko.Types
import           Game.Osu.OszLoader.Types

run ∷ Game a → IO (Maybe a)
run = runGame (def ^. windowMode) (def ^. resolution . unR)

dir ∷ FilePath
dir = "/home/shana/oszs"

fontdir ∷ FilePath
fontdir = "/home/shana/programming/free-taiko/data/fonts/VL-PGothic-Regular.ttf"

loadRes ∷ MonadIO m ⇒ m Resources
loadRes = do
  fnt ← loadFont fontdir
  return $ Res { _font = fnt }

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
    Just m → loadRes >>= evalStateT menuLoop . mkMenu m
  where
    menuLoop ∷ MenuLoop ()
    menuLoop = do
      whenM (keyPress KeyEscape) $ quit .= True
      whenM (keyPress KeyUp) $ screenState . maps %= C.previous
      whenM (keyPress KeyDown) $ screenState . maps %= C.next
      fnt ← use (resources . font)
      bmaps ← use (screenState . maps)
      renderResult 15 2 fnt bmaps
      q ← use quit
      fps ← getFPS

      color (Color 255 0 0 255) $ translate (V2 5 5) $ text fnt 5 (show fps)


      tick >> unless q menuLoop
