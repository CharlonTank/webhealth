module HtmlQuery exposing
    ( allElements
    , attr
    , findAll
    , findFirst
    , parse
    , tagName
    , textOf
    )

import Html.Parser as P exposing (Node(..))


parse : String -> List Node
parse html =
    case P.run html of
        Ok nodes ->
            nodes

        Err _ ->
            []


tagName : Node -> Maybe String
tagName n =
    case n of
        Element name _ _ ->
            Just (String.toLower name)

        _ ->
            Nothing


attr : String -> Node -> Maybe String
attr key n =
    case n of
        Element _ attrs _ ->
            attrs
                |> List.filter (\( k, _ ) -> String.toLower k == String.toLower key)
                |> List.head
                |> Maybe.map Tuple.second

        _ ->
            Nothing


textOf : Node -> String
textOf n =
    case n of
        Text s ->
            s

        Element _ _ children ->
            String.concat (List.map textOf children)

        Comment _ ->
            ""


allElements : List Node -> List Node
allElements nodes =
    List.concatMap walk nodes


walk : Node -> List Node
walk n =
    case n of
        Element _ _ children ->
            n :: List.concatMap walk children

        _ ->
            []


findFirst : (Node -> Bool) -> List Node -> Maybe Node
findFirst pred nodes =
    nodes
        |> allElements
        |> List.filter pred
        |> List.head


findAll : (Node -> Bool) -> List Node -> List Node
findAll pred nodes =
    nodes
        |> allElements
        |> List.filter pred
