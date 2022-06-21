module Markup.Html.Keyed exposing (custom)

{-|


# Html

@docs custom

-}

import Markup
import Markup.Html exposing (Attribute, Html, Key, Tag)


custom : Tag -> List Attribute -> List ( Key, Html ) -> Html
custom =
    Markup.tagNode
