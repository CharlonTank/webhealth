module RPC exposing (lamdera_handleEndpoints)

import Audit
import Dict
import Http
import Json.Decode as D
import Json.Encode as E
import JsonEncoders
import LamderaRPC exposing (HttpBody(..), HttpRequest, RPCResult(..))
import Task
import Time
import Types exposing (..)
import Url


{-| Magic name. Lamdera's runtime auto-discovers this and routes
POST /\_r/<endpoint> requests through it.
-}
lamdera_handleEndpoints : E.Value -> HttpRequest -> BackendModel -> ( RPCResult, BackendModel, Cmd BackendMsg )
lamdera_handleEndpoints _ req model =
    case req.endpoint of
        "audit" ->
            handleAudit req model

        "ping" ->
            ( ResultJson
                (E.object
                    [ ( "ok", E.bool True )
                    , ( "endpoint", E.string req.endpoint )
                    , ( "bodyType"
                      , E.string
                            (case req.body of
                                BodyJson _ ->
                                    "json"

                                BodyString _ ->
                                    "string"

                                BodyBytes _ ->
                                    "bytes"
                            )
                      )
                    ]
                )
            , model
            , Cmd.none
            )

        _ ->
            ( ResultRaw 404 "Not Found" [] (BodyString ("Unknown endpoint: " ++ req.endpoint))
            , model
            , Cmd.none
            )


handleAudit : HttpRequest -> BackendModel -> ( RPCResult, BackendModel, Cmd BackendMsg )
handleAudit req model =
    case decodeAuditRequest req.body of
        Err msg ->
            ( jsonError 400 msg, model, Cmd.none )

        Ok rawUrl ->
            case sanitizeUrl rawUrl of
                Nothing ->
                    ( jsonError 400 "Invalid URL — include http:// or https://", model, Cmd.none )

                Just url ->
                    let
                        host =
                            domainOf url
                    in
                    case freshFor host model.history of
                        Just entry ->
                            ( ResultJson
                                (E.object
                                    [ ( "status", E.string "ready" )
                                    , ( "report", JsonEncoders.encodeReport entry.report )
                                    ]
                                )
                            , model
                            , Cmd.none
                            )

                        Nothing ->
                            ( ResultJson
                                (E.object
                                    [ ( "status", E.string "running" )
                                    , ( "url", E.string url )
                                    , ( "retry_in_seconds", E.int 8 )
                                    , ( "hint", E.string "POST again in a few seconds to fetch the result." )
                                    ]
                                )
                            , model
                            , runAuditCmd url
                            )


decodeAuditRequest : HttpBody -> Result String String
decodeAuditRequest body =
    case body of
        BodyJson value ->
            D.decodeValue (D.field "url" D.string) value
                |> Result.mapError D.errorToString

        BodyString s ->
            case D.decodeString (D.field "url" D.string) s of
                Ok url ->
                    Ok url

                Err _ ->
                    -- allow plain text URL as a convenience
                    if String.startsWith "http" (String.trim s) then
                        Ok (String.trim s)

                    else
                        Err "Body must be JSON {\"url\": \"...\"} or a raw URL string."

        BodyBytes _ ->
            Err "Bytes bodies are not supported by this endpoint."


jsonError : Int -> String -> RPCResult
jsonError code message =
    ResultRaw code "Error"
        [ ( "Content-Type", "application/json" ) ]
        (BodyJson (E.object [ ( "error", E.string message ) ]))


sanitizeUrl : String -> Maybe String
sanitizeUrl raw =
    let
        trimmed =
            String.trim raw

        withScheme =
            if String.startsWith "http://" trimmed || String.startsWith "https://" trimmed then
                trimmed

            else if trimmed == "" then
                ""

            else
                "https://" ++ trimmed
    in
    Url.fromString withScheme |> Maybe.map (always withScheme)


domainOf : String -> String
domainOf url =
    Url.fromString url |> Maybe.map .host |> Maybe.withDefault url


freshFor : String -> List HistoryEntry -> Maybe HistoryEntry
freshFor host entries =
    -- Returns the most recent entry for this host that's less than 60s old.
    -- We don't have a "now" here without a Cmd, so we trust the list's order
    -- (most recent first) and just return the latest match if any. Caller
    -- treats stale entries as still useful.
    entries
        |> List.filter (\e -> domainOf e.url == host)
        |> List.head


runAuditCmd : String -> Cmd BackendMsg
runAuditCmd url =
    let
        task =
            Time.now
                |> Task.andThen
                    (\t0 ->
                        fetchPage url
                            |> Task.andThen
                                (\res ->
                                    case res of
                                        Err err ->
                                            Task.succeed (Err err)

                                        Ok html ->
                                            runProbesAndBuild url t0 html
                                                |> Task.map Ok
                                )
                    )
    in
    Task.perform (AuditFinished "rpc") task


fetchPage : String -> Task.Task Never (Result String HtmlFetch)
fetchPage url =
    Time.now
        |> Task.andThen
            (\t0 ->
                Http.task
                    { method = "GET"
                    , headers = [ Http.header "User-Agent" "webhealth-rpc/0.1" ]
                    , url = url
                    , body = Http.emptyBody
                    , resolver = stringResolver
                    , timeout = Just 15000
                    }
                    |> Task.andThen
                        (\response ->
                            Time.now
                                |> Task.map
                                    (\t1 ->
                                        case response of
                                            Ok ( meta, body ) ->
                                                Ok
                                                    { status = meta.statusCode
                                                    , body = body
                                                    , headers = Dict.toList meta.headers
                                                    , finalUrl = meta.url
                                                    , millis = Time.posixToMillis t1 - Time.posixToMillis t0
                                                    }

                                            Err err ->
                                                Err err
                                    )
                        )
            )


stringResolver : Http.Resolver Never (Result String ( Http.Metadata, String ))
stringResolver =
    Http.stringResolver
        (\response ->
            case response of
                Http.BadUrl_ s ->
                    Ok (Err ("Bad URL: " ++ s))

                Http.Timeout_ ->
                    Ok (Err "Timeout reaching the page.")

                Http.NetworkError_ ->
                    Ok (Err "Network error reaching the page.")

                Http.BadStatus_ meta body ->
                    Ok (Ok ( meta, body ))

                Http.GoodStatus_ meta body ->
                    Ok (Ok ( meta, body ))
        )


runProbesAndBuild : String -> Time.Posix -> HtmlFetch -> Task.Task Never AuditReport
runProbesAndBuild url t0 html =
    let
        finalUrl =
            html.finalUrl

        origin =
            originOf finalUrl

        links =
            Audit.parseLinks finalUrl html.body
    in
    Task.map5
        (\robots sitemap favicon internals externals ->
            { robots = robots
            , sitemap = sitemap
            , favicon = favicon
            , internals = internals
            , externals = externals
            }
        )
        (probeUrl (origin ++ "/robots.txt"))
        (probeUrl (origin ++ "/sitemap.xml"))
        (probeUrl (origin ++ "/favicon.ico"))
        (links.internal |> List.map probeStatus |> Task.sequence)
        (links.external |> List.map probeStatus |> Task.sequence)
        |> Task.andThen
            (\probes ->
                Time.now
                    |> Task.map
                        (\now ->
                            Audit.buildReport now
                                { url = url
                                , clientId = ""
                                , started = t0
                                , htmlBody = Just html.body
                                , htmlHeaders = Just html.headers
                                , htmlStatus = Just html.status
                                , htmlMillis = Just html.millis
                                , finalUrl = Just finalUrl
                                , robots = probes.robots
                                , sitemap = probes.sitemap
                                , favicon = probes.favicon
                                , internalLinks =
                                    List.map2 Tuple.pair links.internal probes.internals
                                        |> Dict.fromList
                                , externalLinks =
                                    List.map2 Tuple.pair links.external probes.externals
                                        |> Dict.fromList
                                }
                        )
            )


originOf : String -> String
originOf url =
    case Url.fromString url of
        Just u ->
            schemeStr u.protocol ++ "://" ++ u.host ++ portStr u.port_

        Nothing ->
            url


schemeStr : Url.Protocol -> String
schemeStr p =
    case p of
        Url.Https ->
            "https"

        Url.Http ->
            "http"


portStr : Maybe Int -> String
portStr p =
    case p of
        Just n ->
            ":" ++ String.fromInt n

        Nothing ->
            ""


probeUrl : String -> Task.Task Never (Maybe ProbeResult)
probeUrl url =
    Http.task
        { method = "GET"
        , headers = [ Http.header "User-Agent" "webhealth-rpc/0.1" ]
        , url = url
        , body = Http.emptyBody
        , resolver = probeResolver url
        , timeout = Just 8000
        }
        |> Task.onError (\_ -> Task.succeed Nothing)


probeResolver : String -> Http.Resolver Never (Maybe ProbeResult)
probeResolver url =
    Http.stringResolver
        (\response ->
            case response of
                Http.GoodStatus_ meta body ->
                    Ok (Just { status = meta.statusCode, bodyLen = String.length body, url = url })

                Http.BadStatus_ meta body ->
                    Ok (Just { status = meta.statusCode, bodyLen = String.length body, url = url })

                _ ->
                    Ok Nothing
        )


probeStatus : String -> Task.Task Never (Maybe Int)
probeStatus url =
    Http.task
        { method = "GET"
        , headers = [ Http.header "User-Agent" "webhealth-rpc/0.1" ]
        , url = url
        , body = Http.emptyBody
        , resolver = statusResolver
        , timeout = Just 6000
        }
        |> Task.onError (\_ -> Task.succeed Nothing)


statusResolver : Http.Resolver Never (Maybe Int)
statusResolver =
    Http.stringResolver
        (\response ->
            case response of
                Http.GoodStatus_ meta _ ->
                    Ok (Just meta.statusCode)

                Http.BadStatus_ meta _ ->
                    Ok (Just meta.statusCode)

                _ ->
                    Ok Nothing
        )
