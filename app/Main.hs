{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad ( void )
import Control.Concurrent ( threadDelay )
import Data.Functor ( ($>) )
import Data.Text as Text
import Data.String ( IsString (..) )
import Pipes.Prelude ( repeatM )

import GI.Gtk ( Box (..)
              , Button (..)
              , Entry (..)
              , Label (..)
              , Window (..)
              , Orientation (..)
              )

import GI.Gtk( entryGetText )
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple


oneSecond :: Int
oneSecond = 1000000

minute :: Int
minute = 60

workTime :: Int
workTime = minute * 20

breakTime :: Int
breakTime = minute * 5

newtype Goal = Goal Text.Text

instance Show Goal where
    show (Goal t) = Text.unpack t

instance IsString Goal where
    fromString s = Goal . Text.pack $ s

data State = WhatsNext Goal
data Event = StartWorking Goal | UpdateGoal Goal | Tick | Quit

update' :: State -> Event -> Transition State Event
update' (WhatsNext _) (UpdateGoal goal) = Transition (WhatsNext goal) (return Nothing)
update' _ Quit = Exit
update' s _ = Transition s (return Nothing)
-- update' s Tick = Transition (s { seconds = seconds s + 1 } ) (return Nothing)

view' :: State -> AppView Window Event
view' s = bin Window [ #title := "Focus Timer" , on #deleteEvent (const (True, Quit)) ] appState where
    appState = case s of
        WhatsNext goal -> viewWhatsNext goal

viewWhatsNext :: Goal -> Widget Event
viewWhatsNext goal = container Box [ #orientation := OrientationVertical ]
  [ widget Label [ #label := "What's next?" ]
  , widget Entry [ onM #changed (\evBox -> UpdateGoal . Goal <$> entryGetText evBox) ]
  , widget Button [#label := "Quit", on #clicked Quit]
  ]

timer = repeatM $ threadDelay oneSecond $> Tick

main :: IO ()
main = void $ run App { view = view'
               , update = update'
               , inputs = [timer]
               , initialState = WhatsNext ""
               }

