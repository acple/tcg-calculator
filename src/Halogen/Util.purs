module Halogen.Util where

import Prelude

import Halogen.HTML as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

empty :: forall w i. HH.HTML w i
empty = HH.text mempty

fa :: forall w i. String -> Array H.ClassName -> HH.HTML w i
fa s c = HH.i [ HP.classes $ [ H.ClassName ("fa-solid " <> s) ] <> c ] []

fa_ :: forall w i. String -> HH.HTML w i
fa_ s = HH.i [ HP.class_ $ H.ClassName ("fa-solid " <> s) ] []

plusButton :: forall w i. i -> HH.HTML w i
plusButton = button (fa_ "fa-plus") $ H.ClassName "border border-sky-500 text-sky-700 hover:bg-sky-200"

removeButton :: forall w i. i -> HH.HTML w i
removeButton = button (fa_ "fa-xmark") $ H.ClassName "text-gray-500 hover:bg-gray-200"

toggleButton :: forall w i. i -> HH.HTML w i
toggleButton = button (fa_ "fa-arrows-rotate") $ H.ClassName "text-gray-500 hover:bg-gray-200"

upButton :: forall w i. i -> HH.HTML w i
upButton = button (fa_ "fa-angle-up") $ H.ClassName "text-gray-500 hover:bg-gray-200"

downButton :: forall w i. i -> HH.HTML w i
downButton = button (fa_ "fa-angle-down") $ H.ClassName "text-gray-500 hover:bg-gray-200"

button :: forall w i. HH.HTML w i -> H.ClassName -> i -> HH.HTML w i
button content style h =
  HH.button
    [ HP.classes
        [ H.ClassName "rounded w-full h-full py-1 px-2 transition-colors duration-75"
        , style
        ]
    , HE.onClick $ const h
    ]
    [ content ]
