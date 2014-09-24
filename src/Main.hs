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

import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Default
import qualified Data.Text as T
import           FreeGame
import           Game.Osu.FreeTaiko.Menu
import           Game.Osu.FreeTaiko.Types

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
             → [(FilePath, Either T.Text TaikoData)] -- ^ data to render
             → m ()
renderResult fs spacing fnt bmaps = zipWithM_ offset positions (map rend bmaps)
  where
    positions ∷ [Double]
    positions = map (\x → x * fs + spacing) [1 .. ]

    offset ∷ (MonadIO m, Affine m) ⇒ Double → m a → m a
    offset = translate . V2 10

    rend ∷ (FromFinalizer m, Picture2D m, MonadIO m) ⇒ (FilePath, Either a b)
         → m ()
    rend (p, Left _) = color red $ text fnt fs p
    rend (p, Right _) = color green $ text fnt fs p

main ∷ IO ()
main = void . run $ do
  setFPS 60
  setTitle "free-taiko"
  clearColor $ Color 0 0 0 0
  liftM2 mkMenu (runMenu dir) loadRes >>= evalStateT menuLoop
  where
    menuLoop ∷ MenuLoop ()
    menuLoop = do
      whenM (keyPress KeyEscape) $ quit .= True
      fnt ← use (resources . font)
      bmaps ← use (screenState . maps)
      renderResult 15 2 fnt bmaps
      q ← use quit
      fps ← getFPS

      color (Color 255 0 0 255) $ translate (V2 5 5) $ text fnt 5 (show fps)


      tick >> unless q menuLoop
