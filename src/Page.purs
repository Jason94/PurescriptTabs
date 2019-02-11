module Page where

import Prelude

import Data.Array.NonEmpty (fromNonEmpty)
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TabContainer (TabContainerQuery, tabContainer)

-- | Query Algebra

data Query a = DoNothing a

-- | Child related types
data Slot = TabContainerSlot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

-- | Component
tabs = fromNonEmpty (
  { title: "One", html: HH.span_ [ HH.text "Component A" ] }  :|
  [ { title: "Two", html: HH.span_ [ HH.text "Component B" ] }
  , { title: "Three", html: HH.span_ [ HH.text "Component C" ] }
  ])

page :: forall m. H.Component HH.HTML Query Unit Void m
page =
  H.parentComponent
    { initialState: const unit
    , render
    , eval
    , receiver: const Nothing
    }
  where
    render :: Unit -> H.ParentHTML Query TabContainerQuery Slot m
    render _ =
      HH.div_
        [ HH.h1_ [ HH.text "Hello New York City!" ]
        , HH.slot TabContainerSlot (tabContainer tabs) unit (const Nothing)
        ]

    eval :: Query ~> H.ParentDSL Unit Query TabContainerQuery Slot Void m
    eval = case _ of
      DoNothing next -> do
        pure next
