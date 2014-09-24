{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Game.Osu.FreeTaiko.Menu
-- License     :  GPL-3
-- Copyright   :  © Mateusz Kowalczyk, 2014
-- Maintainer  :  fuuzetsu@fuuzetsu.co.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Menu functions

module Game.Osu.FreeTaiko.Menu where

import qualified Data.List.PointedList as PL
import           Codec.Archive.Zip
import           Control.Applicative
import           Control.Lens
import           Control.Monad
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Encoding as E
import           Filesystem.Path.CurrentOS
import           FreeGame
import           Game.Osu.FreeTaiko.Types
import           Game.Osu.OszLoader.OsuParser
import           Game.Osu.OszLoader.Types
import qualified Prelude as P
import           Prelude hiding (FilePath)
import           System.Directory


type TaikoParseResult = Either T.Text TaikoData
type Directory = FilePath

getOszFiles ∷ P.FilePath → IO [P.FilePath]
getOszFiles d = doesDirectoryExist d >>= \case
  True → do
    cs ← map ((decodeString d </>) . decodeString) <$> getDirectoryContents d
    return . map encodeString $ filter ((== Just "osz") . extension) cs
  False → return []

hasExt ∷ T.Text → P.FilePath → Bool
hasExt e = (== Just e) . extension . decodeString

-- | Reads out the .osu file contents from the given 'Archive'.
uncompressOsu ∷ Archive → [(P.FilePath, T.Text)]
uncompressOsu xs =
  let es = filter (hasExt "osu" . eRelativePath) $ zEntries xs
  in map (\x → (eRelativePath x, TL.toStrict . E.decodeUtf8 $ fromEntry x)) es

processMap ∷ OsuMap → Either String TaikoData
processMap m = case _mode . _general $ m of
  1 → Right $ TaikoData { _tdGeneral = _general m
                        , _tdMetadata = _metadata m
                        , _tdDifficulty = _difficulty m
                        , _tdTimingPoints = _timingPoints m
                        , _tdHitObjects = _hitObjects m
                        }
  n → Left $ "Mode " ++ show n ++ " found when looking for taiko instead"

readMaps ∷ P.FilePath → IO [(P.FilePath, Either T.Text TaikoData)]
readMaps d = do
  cs ← map (fmap $ fmap uncompressOsu . toArchiveOrFail) <$> readOsz
  let ts ∷ [(P.FilePath, Either String OsuMap)]
      ts = ffmap (>>= (`parseOsu` Nothing)) $ flt cs

  return $ ffmap (_Left %~ T.pack) (ffmap (>>= processMap) ts)
  where
    flt ∷ [(P.FilePath, Either b [(P.FilePath, a)])]
          → [(P.FilePath, Either b a)]
    flt [] = []
    flt ((p, Left x) : xs) = (p, Left x) : flt xs
    flt ((p, Right x) : xs) = map (\(r, y) → (p <//> r, Right y)) x ++ flt xs

    (<//>) ∷ P.FilePath → P.FilePath → P.FilePath
    x <//> y = encodeString $ decodeString x </> decodeString y

    ffmap ∷ (Functor g, Functor f) => (a -> b) -> f (g a) -> f (g b)
    ffmap = fmap . fmap

    readOsz ∷ IO [(P.FilePath, BL.ByteString)]
    readOsz = getOszFiles d >>= mapM (\x → (x,) <$> BL.readFile x)

runMenu ∷ P.FilePath → Game (Maybe Menu)
runMenu d = liftIO (readMaps d) >>= return . \case
  []   → Nothing
  x:xs → Just $ M { _currentDirectory = d
                  , _maps = PL.PointedList [] x xs
                  }
