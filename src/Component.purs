module TabContainer where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray, head, toArray)
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

type TabData = forall a b.
  { title :: String
  , html :: HH.HTML a b
  }

data TabContainerQuery a = OpenTab String a

type State = { selectedTitle :: String }

select :: String -> State -> State
select title s = s { selectedTitle = title }

tabContainer :: forall m. NonEmptyArray String -> H.Component HH.HTML TabContainerQuery Unit Void m
tabContainer tabTitles =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState =
    { selectedTitle: head tabTitles
    }

  stylingFor :: forall r i. State -> String -> HH.IProp r i
  stylingFor state title = if state.selectedTitle == title
                           then style [ "background-color: goldenrod" ]
                           else style [ "" ]

  renderButton :: State -> String -> H.ComponentHTML TabContainerQuery
  renderButton state title = HH.button
    [ HE.onClick $ HE.input_ (OpenTab title)
    , stylingFor state title
    ]
    [ HH.text title ]

  renderButtons :: State -> H.ComponentHTML TabContainerQuery
  renderButtons state = HH.div_ $ map (renderButton state) (toArray tabTitles)

  render :: State -> H.ComponentHTML TabContainerQuery
  render state =
    HH.div_
      [ HH.h1_ [ HH.text "Hello NY City" ]
      , renderButtons state
      ]

  eval :: TabContainerQuery ~> H.ComponentDSL State TabContainerQuery Void m
  eval = case _ of
    OpenTab title next -> do
      state <- H.get
      _ <- H.modify_ $ select title
      pure next
