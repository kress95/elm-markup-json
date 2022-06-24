module Markup.Html.Lazy exposing (Memo, lazy, init, memo, memoWith)

{-|

@docs Memo, lazy, init, memo, memoWith

-}

import Markup.Html exposing (Html)


type Memo a
    = Memo a Html


lazy : Memo value -> Html
lazy (Memo _ markup) =
    markup


init : (a -> Html) -> a -> Memo a
init view a =
    Memo a (view a)


memo : (a -> Html) -> a -> Memo a -> Memo a
memo view a ((Memo b _) as memoized) =
    if a == b then
        memoized

    else
        Memo a (view a)


memoWith : (a -> a -> Bool) -> (a -> Html) -> a -> Memo a -> Memo a
memoWith equals view a ((Memo b _) as memoized) =
    if equals a b then
        memoized

    else
        Memo a (view a)
