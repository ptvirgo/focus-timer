{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad ( void )
import Control.Concurrent ( threadDelay )
import Data.Functor ( ($>) )
import Data.Text as Text
import Pipes.Prelude ( repeatM )

import GI.Gtk ( Box (..)
              , Button (..)
              , Label (..)
              , Window (..)
              , Orientation (..)
              )

import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple

data State =
  State { seconds :: Integer }

data Event = Tick | Quit

oneSecond :: Int
oneSecond = 1000000

update' :: State -> Event -> Transition State Event
update' s Tick = Transition (s { seconds = seconds s + 1 } ) (return Nothing)
update' _ Quit = Exit

view' :: State -> AppView Window Event
view' s = bin Window
  [ #title := "Focus Timer"
  , on #deleteEvent (const (True, Quit))
  ]
  $ container Box [ #orientation := OrientationVertical ]
  [ widget Label [ #label := ( Text.pack . show . seconds $ s ) ]
  , widget Button [#label := "Quit", on #clicked Quit]
  ]

timer = repeatM $ threadDelay oneSecond $> Tick

main :: IO ()
main = void $ run App { view = view'
               , update = update'
               , inputs = [timer]
               , initialState = State { seconds = 0 }
               }

