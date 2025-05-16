module App.Selector where

import Prelude

import Control.Monad.Maybe.Trans (runMaybeT)
import Data.Array as Array
import Data.Maybe (Maybe(..), maybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Record as Record
import Type.Proxy (Proxy(..))
import Util.Halogen as HU

----------------------------------------------------------------

type Item key = { key :: key, value :: String, selected :: Boolean }

type Items key = Array (Item key)

type Input key = Array { key :: key, value :: String }

type Output key = Array key

data Action key
  = EnterEdit
  | LeaveEdit
  | Toggle key
  | Receive (Input key)

data Query key a
  = SetItems (Items key) a

----------------------------------------------------------------

component :: forall m k. Eq k => H.Component (Query k) (Input k) (Output k) m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = action
      , handleQuery = runMaybeT <<< query
      , receive = Just <<< Receive
      }
  }
  where

  initialState :: _ { items :: Items k, isEditMode :: Boolean }
  initialState = { items: _, isEditMode: false } <<< map (Record.insert (Proxy @"selected") false)

  render { items, isEditMode } =
    HH.div
      [ HP.class_ $ H.ClassName "mx-1 flex" ]
      [ if Array.all (not _.selected) items || isEditMode then renderAddButton else HU.empty
      , renderBackground isEditMode
      , HH.ul
          [ HP.class_ if isEditMode
              then H.ClassName "absolute flex min-w-48 flex-col gap-0.5 bg-white p-4 wrap-anywhere shadow-md"
              else H.ClassName "flex w-full flex-wrap gap-1 wrap-anywhere"
          , HE.onClick $ const EnterEdit
          ]
          $ items <#> renderItem isEditMode
      ]

  renderBackground isEditMode =
    HH.div
      [ HP.class_ if isEditMode then H.ClassName "fixed inset-0 bg-black opacity-20" else H.ClassName "hidden"
      , HE.onClick $ const LeaveEdit
      ]
      []

  renderItem isEditMode item =
    HH.li
      [ HP.class_ if item.selected || isEditMode then H.ClassName "min-w-0" else H.ClassName "hidden" ]
      [ HH.button
          [ HP.classes
              [ H.ClassName "h-full w-full rounded border px-3 py-1 whitespace-pre-wrap transition-colors duration-75"
              , if item.selected
                  then H.ClassName "border-sky-700 bg-sky-500 text-white hover:bg-sky-600"
                  else H.ClassName "border-gray-800 bg-gray-300 text-black hover:bg-gray-400"
              ]
          , HE.onClick $ const (Toggle item.key)
          ]
          [ HH.text item.value ]
      ]

  renderAddButton =
    HH.div_
      [ HU.plusButton EnterEdit ]

  action = case _ of
    EnterEdit -> do
      H.modify_ _ { isEditMode = true }
    LeaveEdit -> do
      { items } <- H.modify _ { isEditMode = false }
      H.raise $ items # Array.filter _.selected <#> _.key
    Toggle key -> do
      whenM (H.gets _.isEditMode) do
        H.modify_ do
          items <- _.items
          let items' = items <#> \item -> if item.key == key then item { selected = not item.selected } else item
          _ { items = items' }
    Receive input -> do
      H.modify_ do
        items <- _.items
        let items' = input <#> \{ key, value } -> { key, value, selected: maybe false _.selected (Array.find (_.key >>> (_ == key)) items) }
        _ { items = items' }

  query :: _ ~> _
  query = case _ of
    SetItems items a -> do
      H.put { items, isEditMode: false }
      pure a
