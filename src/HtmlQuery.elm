module HtmlQuery exposing
    ( allElements
    , attr
    , findAll
    , findFirst
    , parse
    , stripScriptsAndStyles
    , tagName
    , textOf
    )

import Html.Parser as P exposing (Node(..))


parse : String -> List Node
parse html =
    let
        cleaned =
            html
                |> stripScriptsAndStyles
                |> stripComments
                |> stripDoctype

        attempts =
            [ \() -> documentToNodes (P.runDocument html)
            , \() -> documentToNodes (P.runDocument (stripScriptsAndStyles html |> stripComments))
            , \() -> P.run html |> Result.toMaybe
            , \() -> P.run cleaned |> Result.toMaybe
            , \() -> P.run (extractHead cleaned ++ extractBody cleaned) |> Result.toMaybe
            ]
    in
    runAttempts attempts


stripComments : String -> String
stripComments input =
    stripDelimitedFrom "<!--" "-->" input 0


stripDelimitedFrom : String -> String -> String -> Int -> String
stripDelimitedFrom open close input cursor =
    let
        lower =
            String.toLower (String.dropLeft cursor input)
    in
    case String.indexes open lower of
        [] ->
            input

        relStart :: _ ->
            let
                openStart =
                    cursor + relStart

                afterOpenLower =
                    String.toLower (String.dropLeft (openStart + String.length open) input)
            in
            case String.indexes close afterOpenLower of
                [] ->
                    input

                relClose :: _ ->
                    let
                        closeEnd =
                            openStart + String.length open + relClose + String.length close

                        kept =
                            String.left openStart input

                        rebuilt =
                            kept ++ String.dropLeft closeEnd input
                    in
                    stripDelimitedFrom open close rebuilt openStart


stripDoctype : String -> String
stripDoctype input =
    let
        lower =
            String.toLower input
    in
    case String.indexes "<!doctype" lower of
        [] ->
            input

        idx :: _ ->
            case String.indexes ">" (String.dropLeft idx input) of
                [] ->
                    input

                gtRel :: _ ->
                    String.left idx input
                        ++ String.dropLeft (idx + gtRel + 1) input


runAttempts : List (() -> Maybe (List Node)) -> List Node
runAttempts attempts =
    case attempts of
        [] ->
            []

        next :: rest ->
            case next () of
                Just nodes ->
                    if List.isEmpty nodes then
                        runAttempts rest

                    else
                        nodes

                Nothing ->
                    runAttempts rest


documentToNodes : Result a P.Document -> Maybe (List Node)
documentToNodes result =
    case result of
        Ok doc ->
            let
                ( attrs, children ) =
                    doc.document
            in
            Just [ Element "html" attrs children ]

        Err _ ->
            Nothing



-- Strip <script>...</script> and <style>...</style> bodies. The hecrj parser
-- recurses into script/style content and chokes on JS that contains '<' or '>'
-- characters. Replacing the inner content with empty bodies preserves the
-- elements (so we can still see their attributes) without breaking parsing.


stripScriptsAndStyles : String -> String
stripScriptsAndStyles html =
    html
        |> stripBlock "<script" "</script>"
        |> stripBlock "<style" "</style>"


stripBlock : String -> String -> String -> String
stripBlock openPrefix closeTag input =
    stripBlockFrom openPrefix closeTag input 0


stripBlockFrom : String -> String -> String -> Int -> String
stripBlockFrom openPrefix closeTag input cursor =
    let
        lower =
            String.toLower (String.dropLeft cursor input)
    in
    case String.indexes openPrefix lower of
        [] ->
            input

        relStart :: _ ->
            let
                openStart =
                    cursor + relStart

                afterOpenStart =
                    String.dropLeft openStart input
            in
            case String.indexes ">" afterOpenStart of
                [] ->
                    input

                relGt :: _ ->
                    let
                        openEnd =
                            openStart + relGt + 1

                        afterOpenLower =
                            String.toLower (String.dropLeft openEnd input)
                    in
                    case String.indexes closeTag afterOpenLower of
                        [] ->
                            input

                        relClose :: _ ->
                            let
                                closeStart =
                                    openEnd + relClose

                                closeEnd =
                                    closeStart + String.length closeTag

                                kept =
                                    String.left openEnd input

                                closeText =
                                    String.slice closeStart closeEnd input

                                rebuilt =
                                    kept ++ closeText ++ String.dropLeft closeEnd input
                            in
                            stripBlockFrom openPrefix closeTag rebuilt (openEnd + String.length closeTag)


extractHead : String -> String
extractHead html =
    extractSection "<head" "</head>" html


extractBody : String -> String
extractBody html =
    extractSection "<body" "</body>" html


extractSection : String -> String -> String -> String
extractSection openPrefix closeTag input =
    let
        lower =
            String.toLower input
    in
    case String.indexes openPrefix lower of
        [] ->
            ""

        start :: _ ->
            case String.indexes closeTag lower of
                [] ->
                    String.dropLeft start input

                end :: _ ->
                    String.slice start (end + String.length closeTag) input


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
