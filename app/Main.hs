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
  , _canJumpedTwice :: !Bool
  , _dx :: !Float
  , _dy :: !Float
  , _leftPressed :: !Bool
  , _rightPressed :: !Bool
  , _canJump :: !Bool
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

  if keyJumpPressed && ( pl ^. mayJump || pl ^.isOnSolidGround ) && ( pl ^. canJump || not ( pl ^. isOnSolidGround ) )
  then do
    modify ( ( & canJump .~ False ) . ( & dy .~ -10 ) )
    if pl ^. isOnSolidGround
    then
      modify ( & isOnSolidGround .~ False )
    else
      modify ( ( & mayJump .~ False ) . ( & canJumpedTwice .~ False ) )
  else do
    pl <- get
    when ( keyJumpPressed && ( pl ^. dy ) < 0 ) ( modify ( & dy %~ \ x -> x - 0.5 ) )
    modify ( & dy %~ (+) 1.0 )

  pl <- get
  when ( ( pl ^. dy ) > 5 ) ( modify ( & dy .~ 5 ) )

  when keyLeftPressed ( modify ( \ pl -> pl & dx .~ -3 ) )
  when keyRightPressed ( modify ( \ pl -> pl & dx .~ 3 ) )

  modify updatePos

  pl <- get
  let V2 x y = pl ^. pos
  when ( y > 395 && not keyJumpPressed ) ( modify ( & canJump .~ True ) )
  when ( y > 400 ) $
    modify $ ( & pos .~ V2 x 400 )
      . ( & dy .~ 0 )
      . ( & isOnSolidGround .~ True )
      . ( & mayJump .~ False )
      . ( & canJumpedTwice .~ True )

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
  appLoop [] p p renderer
  quit
  where
    p = Player ( V2 300 400 ) True False False 0 0 False False True

keyPressed event code =
  case eventPayload event of
    KeyboardEvent keyboardEvent ->
      keyboardEventKeyMotion keyboardEvent == Pressed &&
      keysymKeycode (keyboardEventKeysym keyboardEvent) == code
    _ -> False

keyReleased event code =
  case eventPayload event of
    KeyboardEvent keyboardEvent ->
      keyboardEventKeyMotion keyboardEvent == Released &&
      keysymKeycode (keyboardEventKeysym keyboardEvent) == code
    _ -> False

draw :: Renderer -> Player -> IO ()
draw renderer player =
  drawRect renderer ( Just ( Rectangle ( P  ( pPos - size ) ) size ) )
  where
    pPos = fmap round $ player ^. pos
    size = V2 20 20

del :: Eq a => [ a ] -> a -> [ a ]
del [] _ = []
del ( x : xs ) a = if x == a then xs else x : del xs a

add :: Eq a => [ a ] -> a -> [ a ]
add [] a = [ a ]
add ( x : xs ) a = if x == a then x : xs else x : add xs a

setKeyState :: [ Event ] -> [ Keycode ] -> Keycode -> [ Keycode ]
setKeyState events keys code
  | any ( `keyPressed` code )  events = add keys code
  | any ( `keyReleased` code ) events = del keys code
  | otherwise                         = keys

appLoop :: [ Keycode ] -> Player -> Player -> Renderer -> IO ()
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

  print ukeys

  e <- getPOSIXTime

  let x = round e - round s
  threadDelay ( 1000 * ( ( 1000 `div` 60 ) - x ) )

  unless qPressed (appLoop ukeys player pl renderer)
