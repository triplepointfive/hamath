{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import SDL hiding (get)
import Linear (V4(..), V2(..))
import Linear.Affine (Point(..))
import Control.Monad (unless)

import Control.Monad.State
import Data.Functor.Identity

import Control.Concurrent ( threadDelay )
import Data.Time.Clock.POSIX ( getPOSIXTime )

import           Control.Lens ((&), (%~), (.~), (^.), makeLenses)

type Position = V2 Float

data Player =
  Player
  { _pos             :: !Position
  , _isOnSolidGround :: !Bool
  , _mayJump :: !Bool
  , _dx :: !Float
  , _dy :: !Float
  }
  deriving ( Show, Eq )

makeLenses ''Player

update :: Bool -> Bool -> Bool -> State Player Player
update keyJumpPressed keyLeftPressed keyRightPressed = do
  isOnGround <- onGround
  if isOnGround
  then do
    modify ( & dx %~ (*) 0.9 )
    if keyJumpPressed
    then
      modify ( ( & dy .~ -10 ) . ( & mayJump .~ False ) . ( & isOnSolidGround .~ False ) )
    else
      modify ( & mayJump .~ True )
  else do
    pl <- get
    when ( keyJumpPressed && ( pl ^. dy ) > 0 ) ( modify ( & dy %~ (-) 0.1 ) )
    modify ( & dy %~ (+) 0.5 )
    pl <- get
    when ( ( pl ^. dy ) > 5 ) ( modify ( & dy .~ 5 ) )

  when keyLeftPressed ( modify ( \ pl -> pl & dx .~ -3 ) )
  when keyRightPressed ( modify ( \ pl -> pl & dx .~ 3 ) )

  modify updatePos

  pl <- get
  let V2 x y = pl ^. pos
  when ( y > 400 ) $
    modify ( ( & pos .~ V2 x 400 ) . ( & dy .~ 0 ) . ( & isOnSolidGround .~ True ) )

  get

updatePos :: Player -> Player
updatePos pl = pl & pos %~ (+) ( V2 ( pl ^. dx ) ( pl ^. dy ) )

onGround :: State Player Bool
onGround = get >>= \p -> return ( p ^.isOnSolidGround )

main :: IO ()
main = do
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  appLoop p p renderer
  quit
  where
    p = Player ( V2 300 400 ) True False 0 0

keyPressed event code =
  case eventPayload event of
    KeyboardEvent keyboardEvent ->
      keyboardEventKeyMotion keyboardEvent == Pressed &&
      keysymKeycode (keyboardEventKeysym keyboardEvent) == code
    _ -> False

draw :: Renderer -> Player -> IO ()
draw renderer player =
  drawRect renderer ( Just ( Rectangle ( P  ( pPos - size ) ) size ) )
  where
    pPos = fmap round $ player ^. pos
    size = V2 20 20

appLoop :: Player -> Player -> Renderer -> IO ()
appLoop prevPlayer player renderer = do
  s <- getPOSIXTime
  events <- pollEvents
  let qPressed = any ( `keyPressed` KeycodeQ ) events
      uPressed = any ( `keyPressed` KeycodeUp ) events
      rPressed = any ( `keyPressed` KeycodeRight ) events
      lPressed = any ( `keyPressed` KeycodeLeft ) events
      pl = evalState ( update uPressed lPressed rPressed ) player

  rendererDrawColor renderer $= V4 0 0 255 255
  clear renderer
  rendererDrawColor renderer $= V4 0 0 0 0

  draw renderer pl

  when ( prevPlayer /= pl ) ( print pl )

  present renderer

  e <- getPOSIXTime


  let x = round e - round s
  threadDelay ( 1000 * ( ( 1000 `div` 60 ) - x ) )

  unless qPressed (appLoop player pl renderer)
