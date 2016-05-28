{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Hamath.Map where

import Data.Aeson
import Linear.Affine ( Point(..) )

type Rect = ( Float, Float, Float, Float )

data Obstacle = Obstacle !Float !Float !Float !Float
  deriving ( Show, Eq )

class Collisionable a where
  toRect :: a -> Rect

  intersect :: Collisionable b => a -> b -> Bool
  intersect a b = not $
    ( x1 > x2 + w2 ) || ( x1 + w1 < x2 ) ||
    ( y1 > y2 + h2 ) || ( y1 + h1 < y2 )
    where
      ( x1, y1, w1, h1 ) = toRect a
      ( x2, y2, w2, h2 ) = toRect b

instance Collisionable Obstacle where
  toRect ( Obstacle x y w h ) = ( x, y, w, h )

data Map = Map
  { obstacles :: ![ Obstacle ]
  } deriving ( Show, Eq )

mapCollise :: Collisionable a => Map -> a -> Bool
mapCollise Map{..} obj = any ( intersect obj ) obstacles

data Tileset
  = Tileset
  { columns    :: Int
  , firstgid   :: Int
  , imagePath  :: FilePath
  , name       :: String
  } deriving ( Show, Eq )

data Layer
  = Layer
  { layerName :: String
  , datum     :: [ Int ]
  } deriving ( Show, Eq )

data Chunk
  = Chunk
  { nextobjectid :: Int
  , tilesets     :: [ Tileset ]
  , layers       :: [ Layer ]
  } deriving ( Show, Eq )

instance FromJSON Tileset where
  parseJSON ( Object v ) = Tileset
    <$> v .: "columns"
    <*> v .: "firstgid"
    <*> v .: "image"
    <*> v .: "name"

instance FromJSON Layer where
  parseJSON ( Object v ) = Layer
    <$> v .: "name"
    <*> v .: "data"

instance FromJSON Chunk where
  parseJSON ( Object v ) = Chunk
    <$> v .: "nextobjectid"
    <*> v .: "tilesets"
    <*> v .: "layers"
