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

type TabDatum =
  { title :: String
  , html :: HH.PlainHTML
  }
type TabData = NonEmptyArray TabDatum

tabTitles :: TabData -> NonEmptyArray String
tabTitles arr = map (\x -> x.title) arr

data TabContainerQuery a = OpenTab TabDatum a

type State =
  { selectedTab :: Int
  , tabDatum :: TabDatum
  }

select :: TabDatum -> State
select tabDatum = { selectedTab: 0, tabDatum: tabDatum }

tabContainer :: forall m. TabData -> H.Component HH.HTML TabContainerQuery Unit Void m
tabContainer tabData =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState =
    { selectedTab: 0
    , tabDatum: head tabData
    }

  stylingFor :: forall r i. State -> String -> HH.IProp r i
  stylingFor state title = if state.tabDatum.title == title
                           then style [ "background-color: goldenrod" ]
                           else style [ "" ]

  renderButton :: State -> TabDatum -> H.ComponentHTML TabContainerQuery
  renderButton state tabDatum = HH.button
    [ HE.onClick $ HE.input_ (OpenTab tabDatum)
    , stylingFor state tabDatum.title
    ]
    [ HH.text tabDatum.title ]

  renderButtons :: State -> H.ComponentHTML TabContainerQuery
  renderButtons state = HH.div_ $ map (renderButton state) (toArray tabData)

  render :: State -> H.ComponentHTML TabContainerQuery
  render state =
    HH.div_
      [ renderButtons state
      , HH.fromPlainHTML state.tabDatum.html
      ]

  eval :: TabContainerQuery ~> H.ComponentDSL State TabContainerQuery Void m
  eval = case _ of
    OpenTab tabDatum next -> do
      _ <- H.put $ select tabDatum
      pure next
