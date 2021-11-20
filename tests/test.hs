{-# LANGUAGE OverloadedStrings #-}

module Main where 

import Test.Tasty
import Test.Tasty.HUnit

import Helpers

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ testCase "countDownMinutes 20" $ "20:00" @=? ( countDownMinutes $ CountDown (20 * minute) )
    , testCase "countDownMinutes 5" $ "5:00" @=? ( countDownMinutes $ CountDown (5 * minute) )
    , testCase "countDownMinutes 3:47" $ "3:47" @=? ( countDownMinutes $ CountDown (3 * minute + 47) )
    , testCase "countDownMinutes -3:47" $ "-3:47" @=? ( countDownMinutes $ CountDown (- 1 * (3 * minute + 47)) )
    , testCase "countDownMinutes -3:47" $ "-6:02" @=? ( countDownMinutes $ CountDown (- 1 * (6 * minute + 2)) )
    ]
