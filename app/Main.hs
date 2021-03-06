{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad ( void )
import Control.Concurrent ( threadDelay )
import Control.Concurrent.Async ( async )

import Data.ByteString ( ByteString )
import Data.Functor ( ($>) )
import Data.Text as Text
import Data.Time.Clock ( getCurrentTime )

import Pipes.Prelude ( repeatM )

import GI.Gtk ( Box (..)
              , Button (..)
              , Entry (..)
              , entryGetText
              , Grid (..)
              , Label (..)
              , Window (..)
              , Orientation (..)
              , Align (..)
              )

import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk

import GI.Gtk.Declarative 
import GI.Gtk.Declarative.App.Simple
import GI.Gtk.Declarative.Container.Grid as G

import Helpers

{- Application State -}

data State =
    WhatsNext Goal
    | Working Goal CountDown
    | Breaking CountDown

initialState' :: State
initialState' = WhatsNext ""


{- Events -}

data Event = StartWorking Goal | UpdateGoal Goal | Reset | StartBreak | Tick | Quit | Close

tickEverySecond = repeatM $ threadDelay oneSecond $> Tick


{- Update -}

update' :: State -> Event -> Transition State Event
update' _ (StartWorking goal) = Transition (Working goal ( CountDown workTime)) (logAndReturn ("start working: " <> show goal) Nothing)
update' _ Reset = Transition initialState' (logAndReturn "reset" Nothing)
update' _ StartBreak = Transition (Breaking . CountDown $ breakTime) (logAndReturn "break" Nothing)
update' s Quit = Transition s (logAndReturn "quit" $ Just Close)
update' _ Close = Exit

update' (WhatsNext _) (UpdateGoal goal) = Transition (WhatsNext goal) (return Nothing)
update' (Working g c) Tick = Transition (Working g . decr $ c) (return Nothing)
update' (Breaking c) Tick = decrBreak c

update' s _ = Transition s (return Nothing)

decrBreak :: CountDown -> Transition State Event
decrBreak c@(CountDown x) = Transition (Breaking . decr $ c) (return event) where
    event = if x > 1 then Nothing
                     else Just Reset

{- Viewers -}

view' :: State -> AppView Window Event
view' s = bin Window [ #title := "Focus Timer" , on #deleteEvent (const (True, Quit)), #widthRequest := 400, #heightRequest := 100 ] appState where
    appState = case s of
        WhatsNext goal -> viewWhatsNext goal
        Working goal countDown -> viewWorking goal countDown
        Breaking countDown -> viewBreaking countDown

viewWhatsNext :: Goal -> Widget Event
viewWhatsNext goal = container Box [ #orientation := OrientationVertical, #valign := AlignEnd ]
    [ widget Label [ #label := "What's next?", classes [ "title" ] ]
    , widget Entry [ onM #changed (\evBox -> UpdateGoal . Goal <$> entryGetText evBox), on #activate $ StartWorking goal ]
    , container Box
          [ #orientation := OrientationHorizontal, #halign := AlignEnd ]
          [ widget Button [ #label := "start", on #clicked $ StartWorking goal ]
          ]
    ]

viewWorking :: Goal -> CountDown -> Widget Event
viewWorking goal countDown = container Box [ #orientation := OrientationVertical ]
    [ widget Label [ #label := ( Text.pack . show $ goal ), classes [ "title" ] ]
    , widget Label [ #label := countDownMinutes countDown, classes timerClasses ]
    , container Box [ #halign := AlignEnd ]
        [ widget Button [ #label := "switch", on #clicked Reset ]
        , widget Button [ #label := "break", on #clicked StartBreak ]
        ]
    ] where
        timerClasses =
            if overTime countDown then [ "timer", "red" ]
                                  else [ "timer" ]

viewBreaking :: CountDown -> Widget Event
viewBreaking countDown = container Box [ #orientation := OrientationVertical ]
    [ widget Label [ #label := "Break", classes [ "title" ] ]
    , widget Label [ #label := countDownMinutes countDown, classes [ "timer" ]]
    , container Box
        [ #orientation := OrientationHorizontal, #halign := AlignEnd ]
        [ widget Button [ #label := "finish", on #clicked Reset ]
        ]
    ]

{- Styling -}

styles :: ByteString
styles = mconcat
    [ ".timer { font-size: xx-large; font-family: monospace; margin: 5pt 0 10pt 0 }"
    , ".title { font-size: large; margin: 5pt 0; }"
    , ".red { background-color: #e33 }"
    , "button { margin: 0 5pt 5pt 5pt; }"
    , "entry { font-size: large; margin: 5pt 5pt 10pt 5pt; }"
    ]

{- Log to stdout -}

logAndReturn :: String -> a -> IO a
logAndReturn msg ret = do
    when <- show <$> getCurrentTime
    putStrLn $ when <> " " <> msg
    return ret

{- Main -}
main :: IO ()
main = do
    void $ Gtk.init Nothing

    screen <- maybe (fail "No screen?!") return =<< Gdk.screenGetDefault
    p <- Gtk.cssProviderNew
    Gtk.cssProviderLoadFromData p styles
  
    Gtk.styleContextAddProviderForScreen
        screen
        p
        (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER)

    void . async $ do
      void $ runLoop app
      Gtk.mainQuit
    Gtk.main
    where
        app = App { view = view' , update = update' , inputs = [tickEverySecond] , initialState = initialState' }
