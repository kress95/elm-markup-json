module Markup.Html.Lazy exposing (Memo, lazy, init, memo, memoWith)

{-|

@docs Memo, lazy, init, memo, memoWith

-}

import Markup
import Markup.Html exposing (Html)


type alias Memo value =
    Markup.Memo value


lazy : Memo value -> Html
lazy =
    Markup.lazy


init : (value -> Html) -> value -> Memo value
init =
    Markup.memoInit


memo : (value -> Html) -> value -> Memo value -> Memo value
memo =
    Markup.memo


memoWith : (value -> value -> Bool) -> (value -> Html) -> value -> Memo value -> Memo value
memoWith =
    Markup.memoWith
