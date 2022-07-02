module Main where

import Prelude

import App.App as App
import Effect (Effect)
import Halogen.Aff (awaitBody)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

----------------------------------------------------------------

main :: Effect Unit
main = HA.runHalogenAff do
  body <- awaitBody
  runUI App.component unit body
