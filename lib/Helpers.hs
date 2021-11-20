module Helpers where

import Data.String ( IsString (..) )
import Data.Text as Text

{- Numbers -}
minute :: Int
minute = 60

breakTime :: Int
breakTime = minute * 5

workTime :: Int
workTime = minute * 20

oneSecond :: Int
oneSecond = 1000000

{- Goal Type -}
newtype Goal = Goal Text.Text

instance Show Goal where
    show (Goal t) = Text.unpack t

instance IsString Goal where
    fromString s = Goal . Text.pack $ s

{- CountDown Type -}
newtype CountDown = CountDown Int

countDownMinutes :: CountDown -> Text.Text
countDownMinutes ( CountDown x ) = Text.pack timeLeft where
    minutesLeft = abs x `div` minute
    minutesString =
        if x < 0 then "-" <> show minutesLeft
                 else show minutesLeft

    secondsLeft = abs x `rem` minute
    secondString =
        if secondsLeft < 10 then "0" <> show secondsLeft
                  else show secondsLeft

    timeLeft = minutesString <> ":" <> secondString
