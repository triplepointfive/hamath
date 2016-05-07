{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import SDL hiding ( get )
import Linear ( V4(..), V2(..) )
import Linear.Affine ( Point(..) )
import Control.Monad ( unless )

import Control.Monad.State

import Control.Concurrent ( threadDelay )
import Data.Time.Clock.POSIX ( getPOSIXTime )
import qualified Data.Set as Set

import           Control.Lens ( (&), (%~), (.~), (^.), makeLenses )

import           Hamath.Map
import           Hamath.Draw

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

instance Collisionable Player where
  toRect Player{..} = ( px, py, 20, 20 )
    where
      V2 px py = _pos

obss = [ Obstacle 0 400 800 50
       , Obstacle 0 200 50 200
       , Obstacle 450 200 50 200
       , Obstacle 50 250 150 10
       ]

type AppLoop r = StateT App IO r

data App = App
  { _keys     :: !( Set.Set Keycode )
  , _player   :: !Player
  , _renderer :: !Renderer
  } deriving( Show, Eq )

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
  modify moveHorizontal
  modify moveVertical

  pl <- get
  let V2 x y = pl ^. pos
  when ( pl ^. isOnSolidGround && pl ^. jumpWasReleased ) ( modify ( & canJump .~ True ) )

  get

restrict :: Player -> Player
restrict pl@Player{..}
  = pl & dy %~ min 7 & dx %~ \ dx -> if abs dx < 0.01 then 0 else dx

moveVertical :: Player -> Player
moveVertical pl
  | any ( uPl `intersect` ) obss = pl & dy .~ 0
    & isOnSolidGround .~ True
    & mayJump .~ False
    & canJumpedTwice .~ True
  | otherwise = uPl
  where
    uPl = pl & pos %~ (+) ( V2 0 ( pl ^. dy ) )

moveHorizontal :: Player -> Player
moveHorizontal pl
  | any ( uPl `intersect` ) obss = pl & dx .~ 0
  | otherwise = uPl
  where
    uPl = pl & pos %~ (+) ( V2 ( pl ^. dx ) 0 )

onGround :: State Player Bool
onGround = get >>= \p -> return ( p ^.isOnSolidGround )

main :: IO ()
main = do
  initializeAll
  window <- createWindow "Hamath" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  void $ evalStateT appLoop ( App Set.empty p renderer )
  quit
  where
    p = Player ( V2 300 350 ) True False False 0 0 True False

keyPressed = keyWithState Pressed
keyReleased = keyWithState Released

keyWithState state event code =
  case eventPayload event of
    KeyboardEvent keyboardEvent ->
      keyboardEventKeyMotion keyboardEvent == state &&
      keysymKeycode (keyboardEventKeysym keyboardEvent) == code
    _ -> False

setKeyState :: [ Event ] -> Set.Set Keycode -> Keycode -> Set.Set Keycode
setKeyState events keys code
  | any ( `keyPressed` code )  events = Set.insert code keys
  | any ( `keyReleased` code ) events = Set.delete code keys
  | otherwise                         = keys

updatePlayer :: [Event] -> App -> App
updatePlayer events app@App{..} = app{ _keys = ukeys, _player = pl }
  where
    pl = evalState ( update uPressed lPressed rPressed ) _player
    ukeys = foldl ( setKeyState events ) _keys [ KeycodeUp, KeycodeRight, KeycodeLeft, KeycodeQ ]

    uPressed = KeycodeUp `elem` ukeys
    rPressed = KeycodeRight `elem` ukeys
    lPressed = KeycodeLeft `elem` ukeys

drawScene :: MonadIO m => App -> m ()
drawScene App{..} = do
  rendererDrawColor _renderer $= V4 0 0 255 255
  clear _renderer
  rendererDrawColor _renderer $= V4 0 0 0 0

  draw _renderer _player
  mapM_ ( draw _renderer ) obss

  present _renderer

qPressed :: AppLoop Bool
qPressed = do
  app <- get
  return $ KeycodeQ `Set.member` _keys app

appLoop :: AppLoop ()
appLoop = do
  s <- lift getPOSIXTime
  pollEvents >>= \events -> modify ( updatePlayer events )

  get >>= drawScene

  e <- lift getPOSIXTime

  let x = round e - round s
  lift $ threadDelay ( 1000 * ( ( 1000 `div` 60 ) - x ) )

  q <- qPressed
  unless q appLoop
