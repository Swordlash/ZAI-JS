module Test.Main where

import Prelude
import Test.QuickCheck.Gen (randomSample)
import Types (Book)

import Effect (Effect)
import Effect.Class.Console (logShow)
import Test.QuickCheck.Arbitrary (arbitrary)

main :: Effect Unit
main = do
  generated <- randomSample arbitrary
  logShow (generated :: Array Book)
