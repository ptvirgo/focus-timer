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

initialState' :: State
initialState' = WhatsNext ""


{- Events -}

data Event = StartWorking Goal | UpdateGoal Goal | NewGoal | Tick | Quit

tickEverySecond = repeatM $ threadDelay oneSecond $> Tick


{- Update -}

update' :: State -> Event -> Transition State Event
update' _ (StartWorking goal) = Transition (Working goal ( CountDown workTime )) (return Nothing)
update' _ NewGoal = Transition initialState' (return Nothing) 
update' _ Quit = Exit

update' (WhatsNext _) (UpdateGoal goal) = Transition (WhatsNext goal) (return Nothing)
update' (Working g (CountDown x)) Tick = Transition (Working g (CountDown $ x - 1 )) (return Nothing)

update' s _ = Transition s (return Nothing)


{- Viewers -}

view' :: State -> AppView Window Event
view' s = bin Window [ #title := "Focus Timer" , on #deleteEvent (const (True, Quit)) ] appState where
    appState = case s of
        WhatsNext goal -> viewWhatsNext goal
        Working goal countdown -> viewWorking goal countdown

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
        ]
    ]

{- Main -}
main :: IO ()
main = void $ run App { view = view'
               , update = update'
               , inputs = [tickEverySecond]
               , initialState = initialState'
               }

