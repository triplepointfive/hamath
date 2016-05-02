module Hamath.Map where

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
