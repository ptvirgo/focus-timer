{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad ( void )
import Control.Concurrent ( threadDelay )
import Data.Text

import GI.Gtk ( Box (..)
              , Button (..)
              , Label (..)
              , Window (..)
              )

import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple

data State =
  State { seconds :: Integer }

data Event = Tick | Quit

update' :: State -> Event -> Transition State Event
update' (State s) Tick = Transition (State s) (return Nothing)
update' _ Quit = Exit

view' :: State -> AppView Window Event
view' s = bin Window
  [ #title := "Focus Timer"
  , on #deleteEvent (const (True, Quit))
  ]
  $ container Box []
  [ widget Button [#label := "Quit", on #clicked Quit]
  ]

main :: IO ()
main = void $ run App { view = view'
               , update = update'
               , inputs = []
               , initialState = State { seconds = 0 }
               }
