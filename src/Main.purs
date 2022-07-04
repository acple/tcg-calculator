module Main where

import Prelude

import App.App as App
import Effect (Effect)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)

----------------------------------------------------------------

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI App.component unit body
