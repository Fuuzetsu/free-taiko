{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Game.Osu.FreeTaiko.Types
-- License     :  GPL-3
-- Copyright   :  © Mateusz Kowalczyk, 2014
-- Maintainer  :  fuuzetsu@fuuzetsu.co.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Types used in free-taiko

module Game.Osu.FreeTaiko.Types where

import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Default
import qualified Data.List.PointedList as PL
import qualified Data.Text as T
import           Data.Typeable
import           Data.UnixTime
import           Foreign.C.Types
import           FreeGame
import           Game.Osu.OszLoader.Types

-- | Resolution used by the window.
newtype Resolution = R { _unR ∷ BoundingBox2 }
                     deriving (Show, Eq, Ord, Typeable)

-- | 800x600 default resolution
instance Default Resolution where
  def = R $ Box (V2 0 0) (V2 1600 600)

-- | Makes a 'Resolution', defaulting to 800x600 if either given Int
-- is ≤ 0.
mkR :: (Integral x, Integral y) ⇒ x → y → Resolution
mkR x y | x <= 0 || y <= 0 = def
        | otherwise = R $ Box (V2 0 0) (V2 (fromIntegral x) (fromIntegral y))

makeLenses ''Resolution

data TaikoData = TaikoData { _tdGeneral ∷ General
                           , _tdMetadata ∷ Metadata
                           , _tdDifficulty ∷ Difficulty
                           , _tdTimingPoints ∷ [TimingPoint]
                           , _tdHitObjects ∷ [HitObject]
                           }
                  deriving (Show, Eq)

makeLenses ''TaikoData

data Menu = M
  { _currentDirectory ∷ FilePath
  , _maps ∷ PL.PointedList (FilePath, Either T.Text TaikoData)
  , _picked ∷ Maybe (FilePath, TaikoData)
  } deriving (Show, Eq)

makeLenses ''Menu

-- | State of the window
data WindowState = W
  { _windowMode ∷ WindowMode
  , _resolution ∷ Resolution
  } deriving (Show, Eq)

instance Default WindowState where
  def = W { _windowMode = Windowed
          , _resolution = def
          }

makeLenses ''WindowState

data Resources = Res { _font ∷ Font }

makeLenses ''Resources

data ScreenState a = S { _windowState ∷ WindowState
                       , _quit ∷ Bool
                       , _targetFrameRate ∷ Int
                       , _resources ∷ Resources
                       , _screenState ∷ a
                       }

makeLenses ''ScreenState

mkMenu ∷ Menu → Resources → ScreenState Menu
mkMenu m r = S { _windowState = def
               , _quit = False
               , _targetFrameRate = 60
               , _resources = r
               , _screenState = m
               }

type MenuLoop = StateT (ScreenState Menu) Game

data Don = SmallRed | SmallBlue | BigRed | BigBlue
         deriving (Show, Eq, Enum)

-- | Time-annotated elements
data Annotated a = Annot { _annotTime ∷ !UnixTime
                         , _unAnnot ∷  a
                         } deriving (Show, Eq, Ord)

makeLenses ''Annotated

data SongState = SS
  { _dons ∷ [Annotated Don]
  , _elapsed ∷ !Int
  , _lastTick ∷ !UnixTime
  } deriving (Show, Eq)

makeLenses ''SongState

type SongLoop = StateT (ScreenState SongState) Game
