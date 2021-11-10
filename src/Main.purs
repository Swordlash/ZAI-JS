module Main where

import Prelude

import Effect (Effect)

import Halogen (Component, defaultEval, mkComponent, mkEval) as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI) as HD

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  HD.runUI component unit body

component :: forall q i o m. H.Component q i o m
component = H.mkComponent
  { initialState: identity
  , render: const $ HH.button [ HP.classes [ HH.ClassName "btn", HH.ClassName "btn-default" ] ] [ HH.text "button" ]
  , eval: H.mkEval H.defaultEval
  }

