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
              , Entry (..)
              , Label (..)
              , Window (..)
              , Orientation (..)
              )

import GI.Gtk( entryGetText )
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple

import Helpers

{- Application State -}

data State =
    WhatsNext Goal
    | Working Goal CountDown
    | Breaking CountDown

initialState' :: State
initialState' = WhatsNext ""


{- Events -}

data Event = StartWorking Goal | UpdateGoal Goal | NewGoal | StartBreak | Tick | Quit

tickEverySecond = repeatM $ threadDelay oneSecond $> Tick


{- Update -}

update' :: State -> Event -> Transition State Event
update' _ (StartWorking goal) = Transition (Working goal ( CountDown workTime )) (return Nothing)
update' _ NewGoal = Transition initialState' (return Nothing) 
update' _ StartBreak = Transition (Breaking . CountDown $ breakTime) (return Nothing)
update' _ Quit = Exit

update' (WhatsNext _) (UpdateGoal goal) = Transition (WhatsNext goal) (return Nothing)
update' (Working g c) Tick = Transition (Working g . decr $ c) (return Nothing)
update' (Breaking c) Tick = decrBreak c

update' s _ = Transition s (return Nothing)

decrBreak :: CountDown -> Transition State Event
decrBreak c@(CountDown x) = Transition (Breaking . decr $ c) (return event) where
    event = if x > 1 then Nothing
                     else Just NewGoal

{- Viewers -}

view' :: State -> AppView Window Event
view' s = bin Window [ #title := "Focus Timer" , on #deleteEvent (const (True, Quit)) ] appState where
    appState = case s of
        WhatsNext goal -> viewWhatsNext goal
        Working goal countDown -> viewWorking goal countDown
        Breaking countDown -> viewBreaking countDown

viewWhatsNext :: Goal -> Widget Event
viewWhatsNext goal = container Box [ #orientation := OrientationVertical ]
    [ widget Label [ #label := "What's next?" ]
    , widget Entry [ onM #changed (\evBox -> UpdateGoal . Goal <$> entryGetText evBox) ]
    , container Box
          [ #orientation := OrientationHorizontal ]
          [ widget Button [ #label := "Start", on #clicked $ StartWorking goal ]
          ]
    ]

viewWorking :: Goal -> CountDown -> Widget Event
viewWorking goal countDown = container Box [ #orientation := OrientationVertical ]
    [ widget Label [ #label := ( Text.pack . show $ goal )]
    , widget Label [ #label := countDownMinutes countDown ]
    , container Box
        [ #orientation := OrientationHorizontal ]
        [ widget Button [ #label := "switch", on #clicked NewGoal ]
        , widget Button [ #label := "break", on #clicked StartBreak ]
        ]
    ]

viewBreaking :: CountDown -> Widget Event
viewBreaking countDown = container Box [ #orientation := OrientationVertical ]
    [ widget Label [ #label := "Break" ]
    , widget Label [ #label := countDownMinutes countDown ]
    , container Box
        [ #orientation := OrientationHorizontal ]
        [ widget Button [ #label := "finish", on #clicked NewGoal ]
        ]
    ]

{- Main -}
main :: IO ()
main = void $ run App { view = view'
               , update = update'
               , inputs = [tickEverySecond]
               , initialState = initialState'
               }

