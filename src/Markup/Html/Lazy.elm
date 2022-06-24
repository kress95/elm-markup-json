module Markup.Html.Lazy exposing (init, memo, memoWith)

{-|

@docs init, memo, memoWith

-}

import Markup
import Markup.Html exposing (Html, Memo)


init : (value -> Html) -> value -> Memo value
init =
    Markup.memoInit


memo : (value -> Html) -> value -> Memo value -> Memo value
memo =
    Markup.memo


memoWith : (value -> value -> Bool) -> (value -> Html) -> value -> Memo value -> Memo value
memoWith =
    Markup.memoWith
