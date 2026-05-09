module Backend exposing (..)

import Audit
import Dict exposing (Dict)
import Http
import Lamdera exposing (ClientId, SessionId)
import Task exposing (Task)
import Time
import Types exposing (..)
import Url


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \_ -> Sub.none
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { history = [], inflight = Dict.empty }, Cmd.none )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        AuditFinished clientId (Ok report) ->
            let
                entry =
                    { url = report.url
                    , finalUrl = report.finalUrl
                    , scannedAt = report.scannedAt
                    , score = report.score
                    , passed = report.passed
                    , warnings = report.warnings
                    , errors = report.errors
                    , report = report
                    }

                newHistory =
                    entry :: List.take 199 model.history
            in
            ( { model | history = newHistory }
            , Cmd.batch
                [ Lamdera.sendToFrontend clientId (AuditCompleted report)
                , Lamdera.broadcast (HistoryUpdated newHistory)
                ]
            )

        AuditFinished clientId (Err err) ->
            ( model, Lamdera.sendToFrontend clientId (AuditFailed err) )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend _ clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

        RequestHistory ->
            ( model, Lamdera.sendToFrontend clientId (HistoryUpdated model.history) )

        RequestAudit raw ->
            case sanitizeUrl raw of
                Nothing ->
                    ( model, Lamdera.sendToFrontend clientId (AuditFailed "Invalid URL — please include http:// or https://.") )

                Just url ->
                    ( model
                    , Cmd.batch
                        [ Lamdera.sendToFrontend clientId (AuditStarted url)
                        , runAudit url clientId model
                        ]
                    )


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



-- ────────────────────────────────────────────────────────────────────────────
--  Orchestrator
-- ────────────────────────────────────────────────────────────────────────────


runAudit : String -> ClientId -> Model -> Cmd BackendMsg
runAudit url clientId _ =
    let
        task : Task Never (Result String AuditReport)
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
                            |> Task.onError (\_ -> Task.succeed (Err "Failed to fetch the page."))
                    )
    in
    Task.perform (AuditFinished clientId) task


runProbesAndBuild : String -> Time.Posix -> HtmlFetch -> Task Never AuditReport
runProbesAndBuild url t0 html =
    let
        finalUrl =
            html.finalUrl

        origin =
            originOf finalUrl

        links =
            Audit.parseLinks finalUrl html.body

        probesTask =
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
                (faviconProbe finalUrl html.body)
                (links.internal |> List.map probeStatus |> Task.sequence)
                (links.external |> List.map probeStatus |> Task.sequence)
    in
    probesTask
        |> Task.andThen
            (\probes ->
                Time.now
                    |> Task.map
                        (\now ->
                            let
                                inflight : InflightAudit
                                inflight =
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
                            in
                            Audit.buildReport now inflight
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



-- ────────────────────────────────────────────────────────────────────────────
--  HTTP tasks
-- ────────────────────────────────────────────────────────────────────────────


fetchPage : String -> Task Never (Result String HtmlFetch)
fetchPage url =
    Time.now
        |> Task.andThen
            (\t0 ->
                Http.task
                    { method = "GET"
                    , headers = [ Http.header "User-Agent" "is-ready-for-launch-oss/0.1 (audit)" ]
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


probeUrl : String -> Task Never (Maybe ProbeResult)
probeUrl url =
    Http.task
        { method = "GET"
        , headers = [ Http.header "User-Agent" "is-ready-for-launch-oss/0.1 (audit)" ]
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


faviconProbe : String -> String -> Task Never (Maybe ProbeResult)
faviconProbe pageUrl _ =
    -- We only attempt /favicon.ico fallback on the origin; declared favicons
    -- are validated implicitly by the user looking at the report.
    probeUrl (originOf pageUrl ++ "/favicon.ico")


probeStatus : String -> Task Never (Maybe Int)
probeStatus url =
    Http.task
        { method = "GET"
        , headers = [ Http.header "User-Agent" "is-ready-for-launch-oss/0.1 (audit)" ]
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
