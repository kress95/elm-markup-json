module React.Changeset exposing (Changeset, cast, encode)

import Json.Encode as Encode exposing (Value)
import React.Json
import React.VirtualDom as VirtualDom exposing (VirtualDom(..))


type Changeset
    = Fill VirtualDom
    | Diff (Maybe Value) (Maybe Value) (Maybe Value) (Maybe (List Changeset))
    | None


encode : Changeset -> Value
encode diff =
    case diff of
        Fill tree ->
            VirtualDom.encode tree

        Diff tag props events children ->
            mapOptional "children" (Encode.list encode) children []
                |> optional "events" events
                |> optional "props" props
                |> required "html" false
                |> optional "tag" tag
                |> Encode.object

        None ->
            Encode.null


cast : VirtualDom -> VirtualDom -> Changeset
cast prev curr =
    case curr of
        Text hashC _ ->
            case prev of
                Text hashP _ ->
                    if hashC /= hashP then
                        Fill curr

                    else
                        None

                Node _ _ _ _ _ _ ->
                    Fill curr

        Node hashC tagC propsC eventsC hashChildrenC childrenC ->
            case prev of
                Text _ _ ->
                    Fill curr

                Node hashP tagP propsP eventsP hashChildrenP childrenP ->
                    if hashC /= hashP then
                        let
                            tag =
                                castTag tagP tagC

                            props =
                                castProps propsP propsC

                            events =
                                castProps eventsP eventsC
                        in
                        if hashChildrenC /= hashChildrenP then
                            Diff tag props events (castChildren childrenP childrenC)

                        else
                            Diff tag props events Nothing

                    else
                        None



-- internals


mapOptional : String -> (a -> Value) -> Maybe a -> List ( String, Value ) -> List ( String, Value )
mapOptional key fn maybe list =
    case maybe of
        Just data ->
            ( key, fn data ) :: list

        Nothing ->
            list


optional : String -> Maybe Value -> List ( String, Value ) -> List ( String, Value )
optional key maybe list =
    case maybe of
        Just data ->
            ( key, data ) :: list

        Nothing ->
            list


required : String -> Value -> List ( String, Value ) -> List ( String, Value )
required key value list =
    ( key, value ) :: list


false : Value
false =
    Encode.bool False


castTag : String -> String -> Maybe Value
castTag prev curr =
    if prev /= curr then
        Just (Encode.string curr)

    else
        Nothing


castProps : React.Json.HashedValue -> React.Json.HashedValue -> Maybe Value
castProps prev curr =
    if React.Json.isEqual prev curr then
        Nothing

    else
        Just (React.Json.encode curr)


castChildren : List VirtualDom -> List VirtualDom -> Maybe (List Changeset)
castChildren prev curr =
    let
        ( unchanged, tail, return ) =
            List.foldl castChildrenHelp ( True, prev, [] ) curr
    in
    if unchanged && List.isEmpty tail then
        Nothing

    else
        Just (List.reverse return)


castChildrenHelp : VirtualDom -> ( Bool, List VirtualDom, List Changeset ) -> ( Bool, List VirtualDom, List Changeset )
castChildrenHelp item ( unchanged, prev, curr ) =
    case prev of
        head :: tail ->
            if unchanged then
                case cast head item of
                    None ->
                        ( unchanged, tail, None :: curr )

                    changed ->
                        ( False, tail, changed :: curr )

            else
                ( False, tail, cast head item :: curr )

        _ ->
            ( False, prev, Fill item :: curr )
