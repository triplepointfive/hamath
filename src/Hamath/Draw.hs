{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Hamath.Draw where

import Control.Monad.State ( MonadIO )
import Linear ( V2( .. ) )
import Linear.Affine ( Point(..) )
import SDL ( Renderer, Rectangle( .. ), drawRect )

import Hamath.Map

class Drawable a where
  draw :: MonadIO m => Renderer -> a -> m ()

instance Collisionable a => Drawable a where
  draw renderer obj =
    drawRect renderer ( Just ( Rectangle ( P ppos ) size ) )
    where
      ( x, y, w, h ) = toRect obj
      ppos = round <$> V2 x y
      size = round <$> V2 w h
