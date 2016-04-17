{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import SDL hiding (get)
import Linear (V4(..), V2(..))
import Linear.Affine (Point(..))
import Control.Monad (unless)

import Control.Monad.State

import Control.Concurrent ( threadDelay )
import Data.Time.Clock.POSIX ( getPOSIXTime )
import qualified Data.Set as Set

import           Control.Lens ((&), (%~), (.~), (^.), makeLenses)

type Position = V2 Float

data Player =
  Player
  { _pos             :: !Position
  , _isOnSolidGround :: !Bool
  , _mayJump :: !Bool
  , _canJumpedTwice :: !Bool
  , _dx :: !Float
  , _dy :: !Float
  , _canJump :: !Bool
  , _jumpWasReleased :: !Bool
  }
  deriving ( Show, Eq )

makeLenses ''Player

update :: Bool -> Bool -> Bool -> State Player Player
update keyJumpPressed keyLeftPressed keyRightPressed = do
  pl <- get
  if pl ^. isOnSolidGround
  then
    modify ( & dx %~ (*) 0.5 )
  else do
    when ( not keyJumpPressed && pl ^. canJumpedTwice ) ( modify ( mayJump .~ True ) )
    modify ( & dx %~ (*) 0.9 )

  unless keyJumpPressed ( modify ( & jumpWasReleased .~ True ) )

  if keyJumpPressed && ( pl ^. mayJump || pl ^.isOnSolidGround ) && ( pl ^. canJump || not ( pl ^. isOnSolidGround ) )
  then do
    modify ( ( & canJump .~ False ) . ( & dy .~ -10 ) . ( & jumpWasReleased .~ False ) )
    if pl ^. isOnSolidGround
    then
      modify ( & isOnSolidGround .~ False )
    else
      modify ( ( & mayJump .~ False ) . ( & canJumpedTwice .~ False ) )
  else do
    pl <- get
    when ( keyJumpPressed && ( pl ^. dy ) < 0 ) ( modify ( & dy %~ \ x -> x - 0.5 ) )
    modify ( & dy %~ (+) 1.0 )

  when keyLeftPressed ( modify ( & dx .~ -3 ) )
  when keyRightPressed ( modify ( & dx .~ 3 ) )

  modify restrict
  modify updatePos

  modify collisionDetection

  pl <- get
  let V2 x y = pl ^. pos
  when ( y >= 400 && pl ^. jumpWasReleased ) ( modify ( & canJump .~ True ) )

  get

collisionDetection :: Player -> Player
collisionDetection pl =
  if y > 400 then pl & pos .~ V2 x 400
      & dy .~ 0
      & isOnSolidGround .~ True
      & mayJump .~ False
      & canJumpedTwice .~ True
    else pl
  where
    V2 x y = pl ^. pos

restrict :: Player -> Player
restrict pl@Player{..}
  = pl & dy %~ min 5 & dx %~ \ dx -> if abs dx < 0.01 then 0 else dx

updatePos :: Player -> Player
updatePos pl = pl & pos %~ (+) ( V2 ( pl ^. dx ) ( pl ^. dy ) )

onGround :: State Player Bool
onGround = get >>= \p -> return ( p ^.isOnSolidGround )

main :: IO ()
main = do
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  appLoop Set.empty p p renderer
  quit
  where
    p = Player ( V2 300 400 ) True False False 0 0 True False

keyPressed = keyWithState Pressed
keyReleased = keyWithState Released

keyWithState state event code =
  case eventPayload event of
    KeyboardEvent keyboardEvent ->
      keyboardEventKeyMotion keyboardEvent == state &&
      keysymKeycode (keyboardEventKeysym keyboardEvent) == code
    _ -> False

draw :: Renderer -> Player -> IO ()
draw renderer player =
  drawRect renderer ( Just ( Rectangle ( P  ( pPos - size ) ) size ) )
  where
    pPos = fmap round $ player ^. pos
    size = V2 20 20

setKeyState :: [ Event ] -> Set.Set Keycode -> Keycode -> Set.Set Keycode
setKeyState events keys code
  | any ( `keyPressed` code )  events = Set.insert code keys
  | any ( `keyReleased` code ) events = Set.delete code keys
  | otherwise                         = keys

appLoop :: Set.Set Keycode -> Player -> Player -> Renderer -> IO ()
appLoop keys prevPlayer player renderer = do
  s <- getPOSIXTime
  events <- pollEvents
  let pl = evalState ( update uPressed lPressed rPressed ) player
      ukeys = foldl ( setKeyState events ) keys [ KeycodeUp, KeycodeRight, KeycodeLeft ]

      qPressed = any ( `keyPressed` KeycodeQ ) events
      uPressed = KeycodeUp `elem` ukeys -- any ( `keyPressed` KeycodeUp ) events
      rPressed = KeycodeRight `elem` ukeys
      lPressed = KeycodeLeft `elem` ukeys

  rendererDrawColor renderer $= V4 0 0 255 255
  clear renderer
  rendererDrawColor renderer $= V4 0 0 0 0

  draw renderer pl

  drawRect renderer ( Just ( Rectangle ( P  ( V2 0 400 ) ) ( V2 600 400 ) ) )

  when ( prevPlayer /= pl ) ( print pl )

  present renderer

  e <- getPOSIXTime

  let x = round e - round s
  threadDelay ( 1000 * ( ( 1000 `div` 60 ) - x ) )

  unless qPressed (appLoop ukeys player pl renderer)
