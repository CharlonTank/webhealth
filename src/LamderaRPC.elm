module LamderaRPC exposing
    ( Headers
    , HttpBody(..)
    , HttpRequest
    , RPCResult(..)
    , process
    , requestDecoder
    )

{-| Minimal Lamdera RPC bridge.

Lamdera's runtime decodes incoming HTTP RPC requests into a JSON value with a
fixed wire schema and expects responses encoded the same way. This module
exposes just enough surface to handle JSON endpoints — kept tiny on purpose.

-}

import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E


type alias HttpRequest =
    { sessionId : String
    , endpoint : String
    , requestId : String
    , headers : Dict String String
    , body : HttpBody
    }


type HttpBody
    = BodyBytes (List Int)
    | BodyJson E.Value
    | BodyString String


type RPCResult
    = ResultBytes (List Int)
    | ResultJson E.Value
    | ResultString String
    | ResultRaw Int String (List ( String, String )) HttpBody


type alias Headers =
    Dict String String


requestDecoder : D.Decoder HttpRequest
requestDecoder =
    D.map5 HttpRequest
        (D.field "s" D.string)
        (D.field "e" D.string)
        (D.field "r" D.string)
        (D.field "h" (D.dict D.string))
        (D.field "b" rawBodyDecoder)


rawBodyDecoder : D.Decoder HttpBody
rawBodyDecoder =
    D.oneOf
        [ D.field "i" (D.list D.int) |> D.map BodyBytes
        , D.field "j" D.value |> D.map BodyJson
        , D.field "v" D.value |> D.map BodyJson
        , D.field "vs" D.string |> D.map BodyString
        , D.field "st" D.string |> D.map BodyString
        ]


{-| Top-level RPC dispatcher. Lamdera injects:

  - `rpcOut` : sends an encoded response back to the originating HTTP request
  - `rawReq` : the inbound JSON value (matches `requestDecoder`)
  - `handler` : application-defined endpoint handler

Returns `( newWrappedModel, Cmd )`. The wrapped model has a `userModel` field
holding the actual `BackendModel`.

-}
process :
    (String -> String -> Cmd msg)
    -> (E.Value -> Cmd msg)
    -> E.Value
    -> (E.Value -> HttpRequest -> backendModel -> ( RPCResult, backendModel, Cmd msg ))
    -> { a | userModel : backendModel }
    -> ( { a | userModel : backendModel }, Cmd msg )
process log rpcOut rawReq handler model =
    case D.decodeValue requestDecoder rawReq of
        Ok request ->
            let
                ( result, newUserModel, sideCmds ) =
                    handler rawReq request model.userModel

                respond statusCode statusText headers body =
                    rpcOut
                        (E.object
                            [ ( "t", E.string "qr" )
                            , ( "r", E.string request.requestId )
                            , ( "c", E.int statusCode )
                            , ( "ct", E.string statusText )
                            , ( "h", E.object (List.map (\( k, v ) -> ( k, E.string v )) headers) )
                            , body
                            ]
                        )

                responseCmd =
                    case result of
                        ResultBytes ints ->
                            respond 200 "OK" [] ( "i", E.list E.int ints )

                        ResultJson value ->
                            respond 200 "OK" [] ( "v", value )

                        ResultString value ->
                            respond 200 "OK" [] ( "vs", E.string value )

                        ResultRaw code text headers body ->
                            let
                                bodyTuple =
                                    case body of
                                        BodyBytes ints ->
                                            ( "i", E.list E.int ints )

                                        BodyJson value ->
                                            ( "v", value )

                                        BodyString value ->
                                            ( "vs", E.string value )
                            in
                            respond code text headers bodyTuple
            in
            ( { model | userModel = newUserModel }
            , Cmd.batch [ responseCmd, sideCmds ]
            )

        Err err ->
            ( model, log "rpc" ("decode failed: " ++ D.errorToString err) )
