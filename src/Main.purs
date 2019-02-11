module Main where

import Prelude

import Data.Array.NonEmpty (fromArray, fromNonEmpty, toArray)
import Data.NonEmpty ((:|))
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import TabContainer (tabContainer)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  let tabs = fromNonEmpty (
        { title: "One", html: HH.span_ [ HH.text "Component A" ] }  :|
        [ { title: "Two", html: HH.span_ [ HH.text "Component B" ] }
        , { title: "Three", html: HH.span_ [ HH.text "Component C" ] }
        ])
  runUI (tabContainer tabs) unit body
