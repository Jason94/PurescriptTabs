module Main where

import Prelude

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import TabContainer (tabContainer)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI (tabContainer ["One", "Two", "Three"]) unit body
