module TabContainer where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

---
--- Utils
---

style :: forall r i. Array String -> HH.IProp r i
style styles = HH.attr (HH.AttrName "style") (joinWith ";" styles)

---
--- Component
---

data TabContainerQuery a = ToggleState a

type State = { on :: Boolean }

tabContainer :: forall m. H.Component HH.HTML TabContainerQuery Unit Void m
tabContainer =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { on: false }

  render :: State -> H.ComponentHTML TabContainerQuery
  render state =
    HH.div_
      [ HH.h1_
          [ HH.text "Hello world!" ]
      , HH.p_
          [ HH.text "Why not toggle this button:" ]
      , HH.button
          [ HE.onClick (HE.input_ ToggleState) ]
          [ HH.text
              if not state.on
              then "Don't push me"
              else "I said don't push me!"
          ]
      ]

  eval :: TabContainerQuery ~> H.ComponentDSL State TabContainerQuery Void m
  eval = case _ of
    ToggleState next -> do
      _ <- H.modify (\state -> { on: not state.on })
      pure next
