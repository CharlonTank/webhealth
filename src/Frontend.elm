module Frontend exposing (..)

import Audit
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Dict
import FixPrompt
import Html exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Lamdera
import Ports
import Process
import Svg
import Svg.Attributes as SA
import Svg.Events
import Task
import Time
import Types exposing (..)
import Url exposing (Url)


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = subscriptions
        , view = view
        }


subscriptions : Model -> Sub FrontendMsg
subscriptions _ =
    Time.every 30000 Tick


init : Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    ( { key = key
      , page = pageOf url
      , urlInput = ""
      , status = Idle
      , history = []
      , historyQuery = ""
      , now = Time.millisToPosix 0
      , excludedIssues = []
      , promptCopied = False
      }
    , Cmd.batch
        [ Lamdera.sendToBackend RequestHistory
        , Task.perform Tick Time.now
        ]
    )


pageOf : Url -> Page
pageOf url =
    case url.path of
        "/" ->
            Home

        "/history" ->
            HistoryPage

        path ->
            let
                slug =
                    String.dropLeft 1 path
                        |> stripTrailingSlash
            in
            if isHostLike slug then
                SitePage slug

            else
                Home


stripTrailingSlash : String -> String
stripTrailingSlash s =
    if String.endsWith "/" s then
        String.dropRight 1 s

    else
        s


isHostLike : String -> Bool
isHostLike s =
    String.contains "." s
        && String.all isHostChar s


isHostChar : Char -> Bool
isHostChar c =
    Char.isAlphaNum c || c == '.' || c == '-'


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked req ->
            case req of
                Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                External url ->
                    ( model, Nav.load url )

        UrlChanged url ->
            ( { model | page = pageOf url }, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        UrlInputChanged s ->
            ( { model | urlInput = s }, Cmd.none )

        AnalyzeClicked ->
            if String.trim model.urlInput == "" then
                ( model, Cmd.none )

            else
                let
                    host =
                        hostFromInput model.urlInput
                in
                ( { model
                    | status = Running model.urlInput
                    , excludedIssues = []
                    , promptCopied = False
                    , page = Maybe.map SitePage host |> Maybe.withDefault model.page
                  }
                , Cmd.batch
                    [ Lamdera.sendToBackend (RequestAudit model.urlInput)
                    , case host of
                        Just h ->
                            Nav.pushUrl model.key ("/" ++ h)

                        Nothing ->
                            Cmd.none
                    ]
                )

        HistoryQueryChanged q ->
            ( { model | historyQuery = q }, Cmd.none )

        OpenHistoryEntry entry ->
            let
                host =
                    hostFromInput entry.finalUrl
                        |> orElse (hostFromInput entry.url)
            in
            ( { model
                | page = Maybe.map SitePage host |> Maybe.withDefault Home
                , urlInput = entry.url
                , status = Done entry.report
                , excludedIssues = []
                , promptCopied = False
              }
            , Nav.pushUrl model.key (Maybe.map (\h -> "/" ++ h) host |> Maybe.withDefault "/")
            )

        ToggleIssue id ->
            let
                excluded =
                    if List.member id model.excludedIssues then
                        List.filter ((/=) id) model.excludedIssues

                    else
                        id :: model.excludedIssues
            in
            ( { model | excludedIssues = excluded }, Cmd.none )

        CopyFixPrompt prompt ->
            ( { model | promptCopied = True }
            , Cmd.batch
                [ Ports.copyToClipboard prompt
                , Process.sleep 2000 |> Task.perform (\_ -> PromptCopiedMsg)
                ]
            )

        PromptCopiedMsg ->
            ( { model | promptCopied = False }, Cmd.none )

        Tick t ->
            ( { model | now = t }, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        AuditStarted url ->
            ( { model | status = Running url }, Cmd.none )

        AuditCompleted report ->
            let
                host =
                    hostFromInput report.finalUrl
                        |> orElse (hostFromInput report.url)

                navCmd =
                    case ( host, model.page ) of
                        ( Just h, SitePage current ) ->
                            if current == h then
                                Cmd.none

                            else
                                Nav.pushUrl model.key ("/" ++ h)

                        ( Just h, _ ) ->
                            Nav.pushUrl model.key ("/" ++ h)

                        ( Nothing, _ ) ->
                            Cmd.none
            in
            ( { model | status = Done report, urlInput = report.url }, navCmd )

        AuditFailed err ->
            ( { model | status = Failed err }, Cmd.none )

        HistoryUpdated entries ->
            ( { model | history = entries }, Cmd.none )


hostFromInput : String -> Maybe String
hostFromInput raw =
    let
        trimmed =
            String.trim raw

        withScheme =
            if String.startsWith "http://" trimmed || String.startsWith "https://" trimmed then
                trimmed

            else
                "https://" ++ trimmed
    in
    Url.fromString withScheme |> Maybe.map .host


orElse : Maybe a -> Maybe a -> Maybe a
orElse fallback m =
    case m of
        Just _ ->
            m

        Nothing ->
            fallback



-- ────────────────────────────────────────────────────────────────────────────
--  VIEW
-- ────────────────────────────────────────────────────────────────────────────


view : Model -> Browser.Document FrontendMsg
view model =
    { title = "WebHealth - open-source site audit"
    , body =
        [ Html.node "style" [] [ Html.text styles ]
        , Html.div [ A.class "app" ]
            [ viewHeader model
            , Html.main_ [ A.class "main" ]
                [ case model.page of
                    Home ->
                        viewHome model

                    HistoryPage ->
                        viewHistory model

                    SitePage host ->
                        viewSite model host
                ]
            , viewFooter
            ]
        ]
    }


viewHeader : Model -> Html FrontendMsg
viewHeader model =
    Html.header [ A.class "site-header" ]
        [ Html.a [ A.href "/", A.class "brand" ]
            [ Html.span [ A.class "brand-mark" ] [ Html.text "✦" ]
            , Html.span [] [ Html.text "WebHealth" ]
            , Html.span [ A.class "brand-tag" ] [ Html.text "open source" ]
            ]
        , Html.nav [ A.class "site-nav" ]
            [ navLink (model.page == Home) "/" "Audit"
            , navLink (model.page == HistoryPage) "/history" "History"
            , Html.a
                [ A.href "https://github.com/CharlonTank/webhealth"
                , A.target "_blank"
                , A.rel "noopener"
                , A.class "nav-link"
                ]
                [ Html.text "Source" ]
            ]
        ]


navLink : Bool -> String -> String -> Html FrontendMsg
navLink active href label =
    Html.a
        [ A.href href
        , A.class
            (if active then
                "nav-link nav-link--active"

             else
                "nav-link"
            )
        ]
        [ Html.text label ]


viewFooter : Html msg
viewFooter =
    Html.footer [ A.class "site-footer" ]
        [ Html.text "© 2026 WebHealth (OSS) · "
        , Html.a [ A.href "/history" ] [ Html.text "History" ]
        , Html.text " · "
        , Html.a
            [ A.href "https://github.com/CharlonTank/webhealth"
            , A.target "_blank"
            , A.rel "noopener"
            ]
            [ Html.text "GitHub" ]
        ]



-- HOME ───────────────────────────────────────────────────────────────────────


viewHome : Model -> Html FrontendMsg
viewHome model =
    Html.div [ A.class "home" ]
        [ Html.section [ A.class "hero" ]
            [ Html.h1 [ A.class "hero-title" ] [ Html.text "How healthy is your website?" ]
            , Html.p [ A.class "hero-sub" ]
                [ Html.text "Audit any URL - pre-launch or live - for technical, SEO, and accessibility issues."
                ]
            , viewUrlForm model
            ]
        , case model.status of
            Idle ->
                Html.text ""

            Running url ->
                viewRunning url

            Failed err ->
                Html.section [ A.class "panel panel--error" ]
                    [ Html.h2 [] [ Html.text "Audit failed" ]
                    , Html.p [] [ Html.text err ]
                    ]

            Done report ->
                viewReport model report
        , viewApiSection
        ]


viewApiSection : Html msg
viewApiSection =
    Html.section [ A.class "api-section" ]
        [ Html.h2 [] [ Html.text "Run from CLI, Claude, or any agent" ]
        , Html.p [ A.class "api-lead" ]
            [ Html.text "POST a URL, poll until ready. JSON response. No auth, no API key." ]
        , Html.h3 [] [ Html.text "Trigger an audit" ]
        , codeBlock "curl -X POST https://webhealth.lamdera.app/_r/audit \\\n  -H \"Content-Type: application/json\" \\\n  -d '{\"url\":\"https://your-site.com\"}'"
        , Html.h3 [] [ Html.text "Response - audit running" ]
        , codeBlock "{\"status\":\"running\",\"retry_in_seconds\":8,\"url\":\"https://your-site.com\"}"
        , Html.h3 [] [ Html.text "Response - audit ready (poll the same endpoint after ~10 seconds)" ]
        , codeBlock "{\n  \"status\": \"ready\",\n  \"report\": {\n    \"url\": \"https://your-site.com\",\n    \"finalUrl\": \"https://your-site.com/\",\n    \"score\": 97,\n    \"passed\": 33, \"warnings\": 1, \"errors\": 0,\n    \"perceivedLoadMs\": 317, \"totalTestMs\": 4788,\n    \"categories\": [\n      { \"name\": \"Rendering Architecture\", \"checks\": [\n          { \"id\": \"rendering-mode\", \"name\": \"Rendering Mode\",\n            \"severity\": \"pass\",\n            \"summary\": \"Server-rendered HTML - visible to all clients.\",\n            \"affectedResources\": [], \"howToFix\": null, \"extra\": [] }\n      ] },\n      { \"name\": \"Meta Information\", \"checks\": [ /* … */ ] },\n      { \"name\": \"Content Structure\", \"checks\": [ /* … */ ] },\n      { \"name\": \"Technical Optimization\", \"checks\": [ /* … */ ] },\n      { \"name\": \"Accessibility Basics\", \"checks\": [ /* … */ ] },\n      { \"name\": \"Social & Rich Results\", \"checks\": [ /* … */ ] },\n      { \"name\": \"Links Analysis\", \"checks\": [ /* … */ ] }\n    ]\n  }\n}"
        , Html.h3 [] [ Html.text "One-liner: poll until ready" ]
        , codeBlock "while r=$(curl -s -X POST https://webhealth.lamdera.app/_r/audit \\\n  -H \"Content-Type: application/json\" \\\n  -d '{\"url\":\"https://your-site.com\"}') \\\n  && [ \"$(echo \"$r\" | jq -r .status)\" != \"ready\" ]\ndo sleep 8; done\necho \"$r\" | jq .report"
        , Html.h3 [] [ Html.text "Each individual check has this shape" ]
        , codeBlock "{\n  \"id\": \"title-tag\",\n  \"name\": \"Title Tag\",\n  \"severity\": \"pass\",            // \"pass\" | \"warning\" | \"error\"\n  \"summary\": \"Found 44 characters. Length is optimal.\",\n  \"affectedResources\": [],\n  \"howToFix\": null,\n  \"extra\": []\n}"
        , Html.div [ A.class "api-tips" ]
            [ Html.h3 [] [ Html.text "Tips" ]
            , Html.ul []
                [ Html.li []
                    [ Html.text "The first audit for a URL is cached. To force a fresh re-audit after deploying fixes, vary the URL with a query string: "
                    , Html.code [] [ Html.text "https://your-site.com?_t=42" ]
                    , Html.text " then bump the number."
                    ]
                , Html.li []
                    [ Html.text "The audit fetches your page twice in parallel - once with a browser User-Agent, once with Googlebot - to detect bot-cloaked SSR. Structural checks (h1, headings, landmarks) operate on whichever view actually has content." ]
                , Html.li []
                    [ Html.text "Severity values are "
                    , Html.code [] [ Html.text "\"pass\"" ]
                    , Html.text ", "
                    , Html.code [] [ Html.text "\"warning\"" ]
                    , Html.text ", "
                    , Html.code [] [ Html.text "\"error\"" ]
                    , Html.text ". Score is a 0-100 weighted average."
                    ]
                , Html.li []
                    [ Html.text "Lighthouse-style runtime metrics (LCP, CLS, TBT, JS errors) are not available - those need a headless browser. Everything here is server-side static analysis." ]
                ]
            ]
        ]


codeBlock : String -> Html msg
codeBlock content =
    Html.pre [ A.class "api-code" ]
        [ Html.code [] [ Html.text content ] ]


viewUrlForm : Model -> Html FrontendMsg
viewUrlForm model =
    Html.form
        [ A.class "url-form"
        , E.onSubmit AnalyzeClicked
        ]
        [ Html.label [ A.class "url-label" ]
            [ Html.span [] [ Html.text "Website URL" ]
            , Html.input
                [ A.type_ "text"
                , A.placeholder "https://example.com"
                , A.value model.urlInput
                , A.autofocus True
                , A.spellcheck False
                , A.attribute "autocapitalize" "off"
                , A.attribute "autocomplete" "off"
                , E.onInput UrlInputChanged
                ]
                []
            ]
        , Html.button
            [ A.type_ "submit"
            , A.class "primary"
            , A.disabled (model.status == Running model.urlInput)
            ]
            [ Html.text
                (case model.status of
                    Running _ ->
                        "Analyzing…"

                    _ ->
                        "Analyze"
                )
            ]
        ]


viewRunning : String -> Html msg
viewRunning url =
    Html.section [ A.class "panel panel--running" ]
        [ Html.div [ A.class "spinner" ] []
        , Html.div []
            [ Html.h2 [] [ Html.text "Running audit…" ]
            , Html.p [] [ Html.text url ]
            ]
        ]


viewReport : Model -> AuditReport -> Html FrontendMsg
viewReport model report =
    let
        scoreClass =
            if report.score >= 90 then
                "score score--great"

            else if report.score >= 75 then
                "score score--ok"

            else if report.score >= 50 then
                "score score--warn"

            else
                "score score--bad"

        timing =
            "Scanned just now · "
                ++ msToS report.perceivedLoadMs
                ++ " perceived load · "
                ++ msToS report.totalTestMs
                ++ " total test time"
    in
    Html.section [ A.class "panel panel--report" ]
        [ Html.div [ A.class "report-head" ]
            [ Html.div [ A.class scoreClass ]
                [ Html.div [ A.class "score-num" ] [ Html.text (String.fromInt report.score) ]
                , Html.div [ A.class "score-label" ] [ Html.text (scoreLabel report.score) ]
                ]
            , Html.div [ A.class "report-meta" ]
                [ Html.div [ A.class "report-url" ] [ Html.text report.finalUrl ]
                , Html.div [ A.class "report-timing" ] [ Html.text timing ]
                , Html.div [ A.class "report-counts" ]
                    [ countPill "passed" report.passed "Passed"
                    , countPill "warning" report.warnings "Warnings"
                    , countPill "errored" report.errors "Errors"
                    ]
                ]
            ]
        , viewFixPrompt model report
        , Html.div [ A.class "categories" ]
            (List.map viewCategory report.categories)
        ]


countPill : String -> Int -> String -> Html msg
countPill cls n label =
    Html.div [ A.class ("pill pill--" ++ cls) ]
        [ Html.span [ A.class "pill-num" ] [ Html.text (String.fromInt n) ]
        , Html.span [ A.class "pill-label" ] [ Html.text label ]
        ]


scoreLabel : Int -> String
scoreLabel s =
    if s >= 90 then
        "Excellent"

    else if s >= 75 then
        "Good"

    else if s >= 50 then
        "Needs work"

    else
        "Poor"


viewCategory : Category -> Html FrontendMsg
viewCategory cat =
    Html.section [ A.class "category" ]
        [ Html.h3 [ A.class "category-title" ] [ Html.text cat.name ]
        , Html.div [ A.class "checks" ] (List.map viewCheck cat.checks)
        ]


viewCheck : Check -> Html FrontendMsg
viewCheck c =
    Html.details [ A.class ("check check--" ++ severityClass c.severity) ]
        [ Html.summary []
            [ Html.span [ A.class "check-name" ] [ Html.text c.name ]
            , Html.span [ A.class ("badge badge--" ++ severityClass c.severity) ]
                [ Html.text (severityLabel c.severity) ]
            , Html.span [ A.class "check-summary" ] [ Html.text c.summary ]
            ]
        , Html.div [ A.class "check-body" ]
            [ if List.isEmpty c.affectedResources then
                Html.text ""

              else
                Html.div []
                    [ Html.h4 [] [ Html.text "Affected resources" ]
                    , Html.ul [ A.class "affected" ]
                        (List.map (\r -> Html.li [] [ Html.text r ]) c.affectedResources)
                    ]
            , case c.howToFix of
                Just fix ->
                    Html.div []
                        [ Html.h4 [] [ Html.text "How to fix" ]
                        , Html.p [] [ Html.text fix ]
                        ]

                Nothing ->
                    Html.text ""
            , if List.isEmpty c.extra then
                Html.text ""

              else
                Html.div []
                    (List.map
                        (\( k, v ) ->
                            Html.div [ A.class "extra-block" ]
                                [ Html.h4 [] [ Html.text k ]
                                , Html.pre [] [ Html.text v ]
                                ]
                        )
                        c.extra
                    )
            ]
        ]


severityClass : Severity -> String
severityClass s =
    case s of
        Pass ->
            "passed"

        Warning ->
            "warning"

        Errored ->
            "errored"


severityLabel : Severity -> String
severityLabel s =
    case s of
        Pass ->
            "Pass"

        Warning ->
            "Warning"

        Errored ->
            "Error"



-- FIX PROMPT ──────────────────────────────────────────────────────────────────


viewFixPrompt : Model -> AuditReport -> Html FrontendMsg
viewFixPrompt model report =
    let
        issues =
            report.categories
                |> List.concatMap .checks
                |> List.filter (\c -> c.severity /= Pass)

        included =
            issues
                |> List.filter (\c -> not (List.member c.id model.excludedIssues))

        prompt =
            FixPrompt.build report included
    in
    if List.isEmpty issues then
        Html.text ""

    else
        Html.div [ A.class "fix-prompt" ]
            [ Html.div [ A.class "fix-head" ]
                [ Html.h3 [] [ Html.text "LLM fix prompt" ]
                , Html.span [ A.class "fix-counter" ]
                    [ Html.text (String.fromInt (List.length included) ++ "/" ++ String.fromInt (List.length issues) ++ " issues") ]
                ]
            , Html.div [ A.class "fix-issues" ]
                (List.map (viewIssueToggle model) issues)
            , Html.textarea
                [ A.class "fix-text"
                , A.readonly True
                , A.value prompt
                , A.rows 8
                ]
                []
            , Html.button
                [ A.class "primary"
                , E.onClick (CopyFixPrompt prompt)
                ]
                [ Html.text
                    (if model.promptCopied then
                        "Copied ✓"

                     else
                        "Copy prompt"
                    )
                ]
            ]


viewIssueToggle : Model -> Check -> Html FrontendMsg
viewIssueToggle model c =
    let
        included =
            not (List.member c.id model.excludedIssues)
    in
    Html.label [ A.class "issue-toggle" ]
        [ Html.input
            [ A.type_ "checkbox"
            , A.checked included
            , E.onClick (ToggleIssue c.id)
            ]
            []
        , Html.span [ A.class ("badge badge--" ++ severityClass c.severity) ]
            [ Html.text (severityLabel c.severity) ]
        , Html.span [] [ Html.text c.name ]
        ]



-- HISTORY ────────────────────────────────────────────────────────────────────


viewHistory : Model -> Html FrontendMsg
viewHistory model =
    let
        q =
            String.toLower model.historyQuery

        filtered =
            if q == "" then
                model.history

            else
                List.filter
                    (\e -> String.contains q (String.toLower e.url))
                    model.history
    in
    Html.section [ A.class "history" ]
        [ Html.h2 [] [ Html.text "Recent audits" ]
        , Html.input
            [ A.type_ "search"
            , A.class "search"
            , A.placeholder "Search domains…"
            , A.value model.historyQuery
            , E.onInput HistoryQueryChanged
            ]
            []
        , if List.isEmpty filtered then
            Html.p [ A.class "empty" ] [ Html.text "No audits yet. Run one from the home page." ]

          else
            Html.div [ A.class "history-list" ]
                (List.map (viewHistoryEntry model.now) filtered)
        ]


viewHistoryEntry : Time.Posix -> HistoryEntry -> Html FrontendMsg
viewHistoryEntry now entry =
    Html.button
        [ A.class "history-row"
        , E.onClick (OpenHistoryEntry entry)
        ]
        [ Html.div [ A.class "history-main" ]
            [ Html.div [ A.class "history-domain" ] [ Html.text (domainOf entry.url) ]
            , Html.div [ A.class "history-url" ] [ Html.text entry.url ]
            ]
        , Html.div [ A.class "history-meta" ]
            [ Html.span [ A.class "history-age" ] [ Html.text (relativeTime now entry.scannedAt) ]
            , Html.span [ A.class ("history-score score-" ++ scoreBucket entry.score) ]
                [ Html.text (String.fromInt entry.score) ]
            , Html.span [ A.class "history-counts" ]
                [ Html.span [ A.class "p" ] [ Html.text (String.fromInt entry.passed ++ " P") ]
                , Html.span [ A.class "w" ] [ Html.text (String.fromInt entry.warnings ++ " W") ]
                , Html.span [ A.class "e" ] [ Html.text (String.fromInt entry.errors ++ " E") ]
                ]
            ]
        ]


-- SITE PAGE ──────────────────────────────────────────────────────────────────


viewSite : Model -> String -> Html FrontendMsg
viewSite model host =
    let
        entries =
            entriesForHost host model.history
    in
    Html.div [ A.class "site" ]
        [ Html.section [ A.class "site-head" ]
            [ Html.div []
                [ Html.h1 [ A.class "site-host" ] [ Html.text host ]
                , Html.p [ A.class "site-sub" ]
                    [ Html.text
                        (String.fromInt (List.length entries)
                            ++ (if List.length entries == 1 then
                                    " audit recorded"

                                else
                                    " audits recorded"
                               )
                        )
                    ]
                ]
            , viewUrlForm model
            ]
        , case model.status of
            Running u ->
                viewRunning u

            Failed err ->
                Html.section [ A.class "panel panel--error" ]
                    [ Html.h2 [] [ Html.text "Audit failed" ]
                    , Html.p [] [ Html.text err ]
                    ]

            _ ->
                Html.text ""
        , viewScoreChart entries
        , case ( model.status, latestEntry entries ) of
            ( Done report, _ ) ->
                viewReport model report

            ( _, Just entry ) ->
                viewReport model entry.report

            _ ->
                Html.section [ A.class "panel" ]
                    [ Html.p [] [ Html.text "No audits for this host yet. Type a URL above and run one." ] ]
        , if List.length entries > 1 then
            viewSiteHistoryList model.now entries

          else
            Html.text ""
        ]


entriesForHost : String -> List HistoryEntry -> List HistoryEntry
entriesForHost host entries =
    entries
        |> List.filter (\e -> domainOf e.finalUrl == host || domainOf e.url == host)


latestEntry : List HistoryEntry -> Maybe HistoryEntry
latestEntry entries =
    -- entries are stored most-recent-first
    List.head entries


viewSiteHistoryList : Time.Posix -> List HistoryEntry -> Html FrontendMsg
viewSiteHistoryList now entries =
    Html.section [ A.class "site-history" ]
        [ Html.h2 [] [ Html.text "All audits" ]
        , Html.div [ A.class "history-list" ]
            (List.map (viewHistoryEntry now) entries)
        ]



-- SCORE-OVER-TIME CHART ──────────────────────────────────────────────────────


viewScoreChart : List HistoryEntry -> Html FrontendMsg
viewScoreChart entries =
    if List.length entries < 2 then
        Html.text ""

    else
        let
            chronological =
                entries
                    |> List.sortBy (.scannedAt >> Time.posixToMillis)

            width =
                760

            height =
                220

            padLeft =
                42

            padRight =
                16

            padTop =
                14

            padBottom =
                48

            innerW =
                width - padLeft - padRight

            innerH =
                height - padTop - padBottom

            timestamps =
                chronological |> List.map (.scannedAt >> Time.posixToMillis)

            tMin =
                List.minimum timestamps |> Maybe.withDefault 0

            tMax =
                List.maximum timestamps |> Maybe.withDefault (tMin + 1)

            tSpan =
                max 1 (tMax - tMin)

            xOf t =
                toFloat padLeft + toFloat (t - tMin) / toFloat tSpan * toFloat innerW

            yOf score =
                toFloat padTop + (1 - toFloat score / 100) * toFloat innerH

            points =
                chronological
                    |> List.map
                        (\e ->
                            ( xOf (Time.posixToMillis e.scannedAt), yOf e.score, e )
                        )

            polylinePts =
                points
                    |> List.map (\( x, y, _ ) -> formatFloat x ++ "," ++ formatFloat y)
                    |> String.join " "

            yLabels =
                [ 0, 25, 50, 75, 100 ]

            nXTicks =
                4

            xTickTimes =
                List.range 0 nXTicks
                    |> List.map (\i -> tMin + tSpan * i // nXTicks)

            xTickFormat =
                if tSpan < 24 * 3600 * 1000 then
                    formatTimeOnly

                else if tSpan < 31 * 24 * 3600 * 1000 then
                    formatMonthDay

                else
                    formatMonthDayYear

            chartBaselineY =
                height - padBottom + 1
        in
        Html.section [ A.class "score-chart" ]
            [ Html.h2 [] [ Html.text "Score over time" ]
            , Svg.svg
                [ SA.viewBox ("0 0 " ++ String.fromInt width ++ " " ++ String.fromInt height)
                , SA.class "chart-svg"
                ]
                (List.concat
                    [ List.map (gridLine padLeft (width - padRight) yOf) yLabels
                    , List.map (yAxisLabel padLeft yOf) yLabels
                    , [ Svg.line
                            [ SA.x1 (String.fromInt padLeft)
                            , SA.x2 (String.fromInt (width - padRight))
                            , SA.y1 (String.fromInt chartBaselineY)
                            , SA.y2 (String.fromInt chartBaselineY)
                            , SA.class "chart-axis"
                            ]
                            []
                      ]
                    , List.map (xTick xOf chartBaselineY xTickFormat) xTickTimes
                    , [ Svg.polyline
                            [ SA.points polylinePts
                            , SA.class "chart-line"
                            , SA.fill "none"
                            ]
                            []
                      ]
                    , List.map chartDot points
                    ]
                )
            ]


xTick : (Int -> Float) -> Int -> (Time.Posix -> String) -> Int -> Svg.Svg msg
xTick xOf baselineY fmt t =
    let
        x =
            xOf t
    in
    Svg.g [ SA.class "chart-x-tick" ]
        [ Svg.line
            [ SA.x1 (formatFloat x)
            , SA.x2 (formatFloat x)
            , SA.y1 (String.fromInt baselineY)
            , SA.y2 (String.fromInt (baselineY + 5))
            , SA.class "chart-axis"
            ]
            []
        , Svg.text_
            [ SA.x (formatFloat x)
            , SA.y (String.fromInt (baselineY + 18))
            , SA.textAnchor "middle"
            , SA.class "chart-axis-label"
            ]
            [ Svg.text (fmt (Time.millisToPosix t)) ]
        ]


formatTimeOnly : Time.Posix -> String
formatTimeOnly t =
    let
        zone =
            Time.utc

        hour =
            String.fromInt (Time.toHour zone t) |> String.padLeft 2 '0'

        minute =
            String.fromInt (Time.toMinute zone t) |> String.padLeft 2 '0'
    in
    hour ++ ":" ++ minute


formatMonthDay : Time.Posix -> String
formatMonthDay t =
    let
        zone =
            Time.utc
    in
    monthShort (Time.toMonth zone t) ++ " " ++ String.fromInt (Time.toDay zone t)


formatMonthDayYear : Time.Posix -> String
formatMonthDayYear t =
    let
        zone =
            Time.utc
    in
    monthShort (Time.toMonth zone t)
        ++ " "
        ++ String.fromInt (Time.toDay zone t)
        ++ ", "
        ++ String.fromInt (Time.toYear zone t)


monthShort : Time.Month -> String
monthShort m =
    case m of
        Time.Jan ->
            "Jan"

        Time.Feb ->
            "Feb"

        Time.Mar ->
            "Mar"

        Time.Apr ->
            "Apr"

        Time.May ->
            "May"

        Time.Jun ->
            "Jun"

        Time.Jul ->
            "Jul"

        Time.Aug ->
            "Aug"

        Time.Sep ->
            "Sep"

        Time.Oct ->
            "Oct"

        Time.Nov ->
            "Nov"

        Time.Dec ->
            "Dec"


gridLine : Int -> Int -> (Int -> Float) -> Int -> Svg.Svg msg
gridLine x1_ x2_ yOf score =
    Svg.line
        [ SA.x1 (String.fromInt x1_)
        , SA.x2 (String.fromInt x2_)
        , SA.y1 (formatFloat (yOf score))
        , SA.y2 (formatFloat (yOf score))
        , SA.class "chart-grid"
        ]
        []


yAxisLabel : Int -> (Int -> Float) -> Int -> Svg.Svg msg
yAxisLabel padLeft yOf score =
    Svg.text_
        [ SA.x (String.fromInt (padLeft - 8))
        , SA.y (formatFloat (yOf score + 4))
        , SA.class "chart-axis-label"
        , SA.textAnchor "end"
        ]
        [ Svg.text (String.fromInt score) ]


chartDot : ( Float, Float, HistoryEntry ) -> Svg.Svg FrontendMsg
chartDot ( x, y, entry ) =
    let
        label =
            formatDateTime entry.scannedAt ++ "  ·  " ++ String.fromInt entry.score ++ "/100"

        labelWidth =
            toFloat (String.length label * 6 + 20)

        tooltipX =
            x - labelWidth / 2

        tooltipY =
            y - 36
    in
    Svg.g
        [ SA.class ("chart-dot chart-dot--" ++ scoreBucket entry.score)
        , Svg.Events.onClick (OpenHistoryEntry entry)
        ]
        [ Svg.circle
            [ SA.cx (formatFloat x)
            , SA.cy (formatFloat y)
            , SA.r "5"
            , SA.class "chart-dot-circle"
            ]
            []
        , Svg.g [ SA.class "chart-tooltip" ]
            [ Svg.rect
                [ SA.x (formatFloat tooltipX)
                , SA.y (formatFloat tooltipY)
                , SA.width (formatFloat labelWidth)
                , SA.height "22"
                , SA.rx "4"
                , SA.class "chart-tooltip-bg"
                ]
                []
            , Svg.text_
                [ SA.x (formatFloat x)
                , SA.y (formatFloat (tooltipY + 15))
                , SA.textAnchor "middle"
                , SA.class "chart-tooltip-text"
                ]
                [ Svg.text label ]
            ]
        ]


formatDateTime : Time.Posix -> String
formatDateTime t =
    let
        zone =
            Time.utc

        month =
            case Time.toMonth zone t of
                Time.Jan ->
                    "Jan"

                Time.Feb ->
                    "Feb"

                Time.Mar ->
                    "Mar"

                Time.Apr ->
                    "Apr"

                Time.May ->
                    "May"

                Time.Jun ->
                    "Jun"

                Time.Jul ->
                    "Jul"

                Time.Aug ->
                    "Aug"

                Time.Sep ->
                    "Sep"

                Time.Oct ->
                    "Oct"

                Time.Nov ->
                    "Nov"

                Time.Dec ->
                    "Dec"

        day =
            String.fromInt (Time.toDay zone t)

        hour =
            String.fromInt (Time.toHour zone t) |> String.padLeft 2 '0'

        minute =
            String.fromInt (Time.toMinute zone t) |> String.padLeft 2 '0'
    in
    month ++ " " ++ day ++ " " ++ hour ++ ":" ++ minute


formatFloat : Float -> String
formatFloat f =
    String.fromFloat (toFloat (round (f * 10)) / 10)



-- ─────────────────────────────────────────────────────────────────────────────


scoreBucket : Int -> String
scoreBucket s =
    if s >= 90 then
        "great"

    else if s >= 75 then
        "ok"

    else if s >= 50 then
        "warn"

    else
        "bad"


domainOf : String -> String
domainOf url =
    Url.fromString url
        |> Maybe.map .host
        |> Maybe.withDefault url


relativeTime : Time.Posix -> Time.Posix -> String
relativeTime now then_ =
    let
        delta =
            (Time.posixToMillis now - Time.posixToMillis then_) // 1000
    in
    if delta < 60 then
        "just now"

    else if delta < 3600 then
        String.fromInt (delta // 60) ++ " minutes ago"

    else if delta < 86400 then
        String.fromInt (delta // 3600) ++ " hours ago"

    else
        String.fromInt (delta // 86400) ++ " days ago"


msToS : Int -> String
msToS ms =
    let
        s =
            toFloat ms / 1000

        rounded =
            toFloat (round (s * 100)) / 100
    in
    String.fromFloat rounded ++ "s"



-- STYLES ─────────────────────────────────────────────────────────────────────


styles : String
styles =
    """
:root {
  --bg: #0e0f12;
  --surface: #15171d;
  --surface-2: #1c1f27;
  --border: #2a2e3a;
  --text: #e8eaed;
  --text-dim: #9aa0aa;
  --accent: #4ed391;
  --warn: #f5a524;
  --err: #ef4444;
  --pass: #4ed391;
}

* { box-sizing: border-box; }
html, body { margin: 0; padding: 0; }
body {
  background: var(--bg);
  color: var(--text);
  font-family: -apple-system, BlinkMacSystemFont, "SF Pro Text", "Segoe UI", system-ui, sans-serif;
  font-size: 15px;
  line-height: 1.5;
}
a { color: inherit; text-decoration: none; }
button { font: inherit; cursor: pointer; }

.app { min-height: 100vh; display: flex; flex-direction: column; }
.main { flex: 1; max-width: 980px; width: 100%; margin: 0 auto; padding: 32px 20px 80px; }

.site-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 14px 24px;
  border-bottom: 1px solid var(--border);
  position: sticky;
  top: 0;
  background: rgba(14, 15, 18, 0.85);
  backdrop-filter: blur(8px);
  z-index: 10;
}
.brand {
  display: flex;
  gap: 8px;
  align-items: center;
  font-weight: 600;
}
.brand-mark { color: var(--accent); }
.brand-tag {
  font-size: 11px;
  padding: 2px 6px;
  border: 1px solid var(--border);
  border-radius: 4px;
  color: var(--text-dim);
  font-weight: 500;
}
.site-nav { display: flex; gap: 6px; }
.nav-link {
  padding: 8px 14px;
  border-radius: 6px;
  color: var(--text-dim);
  font-weight: 500;
}
.nav-link:hover { color: var(--text); background: var(--surface); }
.nav-link--active { color: var(--text); background: var(--surface); }

.hero { text-align: center; padding: 40px 0 32px; }
.hero-title { font-size: 36px; margin: 0 0 12px; letter-spacing: -0.02em; }
.hero-sub { color: var(--text-dim); margin: 0 auto 28px; max-width: 560px; }

.url-form {
  display: flex;
  gap: 8px;
  max-width: 640px;
  margin: 0 auto;
  align-items: flex-end;
}
.url-label { flex: 1; text-align: left; }
.url-label > span { display: block; font-size: 12px; color: var(--text-dim); margin-bottom: 6px; }
.url-form input {
  width: 100%;
  padding: 12px 14px;
  background: var(--surface);
  color: var(--text);
  border: 1px solid var(--border);
  border-radius: 8px;
  font-size: 15px;
}
.url-form input:focus { outline: none; border-color: var(--accent); }
.primary {
  background: var(--accent);
  color: #08130c;
  border: 0;
  padding: 12px 22px;
  border-radius: 8px;
  font-weight: 600;
}
.primary:hover { filter: brightness(1.05); }
.primary:disabled { opacity: 0.6; cursor: progress; }

.panel { margin-top: 32px; background: var(--surface); border: 1px solid var(--border); border-radius: 12px; padding: 24px; }
.panel--error { border-color: var(--err); }
.panel--running { display: flex; gap: 16px; align-items: center; }

.spinner {
  width: 24px; height: 24px;
  border: 3px solid var(--border);
  border-top-color: var(--accent);
  border-radius: 50%;
  animation: spin 0.8s linear infinite;
}
@keyframes spin { to { transform: rotate(360deg); } }

.report-head {
  display: flex;
  gap: 24px;
  align-items: center;
  flex-wrap: wrap;
}
.score {
  width: 96px; height: 96px;
  border-radius: 50%;
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  flex-shrink: 0;
  border: 4px solid var(--accent);
}
.score--great { border-color: var(--pass); }
.score--ok { border-color: #fbbf24; }
.score--warn { border-color: var(--warn); }
.score--bad { border-color: var(--err); }
.score-num { font-size: 28px; font-weight: 700; }
.score-label { font-size: 11px; color: var(--text-dim); }
.report-meta { flex: 1; min-width: 240px; }
.report-url { font-size: 18px; font-weight: 600; word-break: break-all; }
.report-timing { font-size: 12px; color: var(--text-dim); margin: 4px 0 12px; }
.report-counts { display: flex; gap: 8px; }
.pill {
  display: inline-flex;
  gap: 6px;
  align-items: baseline;
  padding: 6px 12px;
  border-radius: 999px;
  background: var(--surface-2);
  border: 1px solid var(--border);
}
.pill-num { font-weight: 700; }
.pill-label { font-size: 12px; color: var(--text-dim); }
.pill--passed { border-color: var(--pass); }
.pill--warning { border-color: var(--warn); }
.pill--errored { border-color: var(--err); }

.fix-prompt {
  margin: 24px 0;
  padding: 16px;
  border: 1px solid var(--border);
  border-radius: 10px;
  background: var(--surface-2);
}
.fix-head { display: flex; justify-content: space-between; align-items: center; margin-bottom: 12px; }
.fix-counter { font-size: 12px; color: var(--text-dim); }
.fix-issues {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(240px, 1fr));
  gap: 6px;
  margin-bottom: 12px;
  max-height: 220px;
  overflow: auto;
  padding-right: 4px;
}
.issue-toggle {
  display: flex;
  gap: 8px;
  align-items: center;
  font-size: 13px;
  padding: 4px;
  border-radius: 4px;
  cursor: pointer;
}
.issue-toggle:hover { background: var(--surface); }
.fix-text {
  width: 100%;
  background: var(--bg);
  color: var(--text);
  border: 1px solid var(--border);
  border-radius: 6px;
  padding: 10px;
  font-family: ui-monospace, SFMono-Regular, Menlo, monospace;
  font-size: 12px;
  margin-bottom: 12px;
  resize: vertical;
}

.categories { margin-top: 32px; display: flex; flex-direction: column; gap: 24px; }
.category-title { margin: 0 0 12px; font-size: 14px; text-transform: uppercase; letter-spacing: 0.06em; color: var(--text-dim); }
.checks { display: flex; flex-direction: column; gap: 6px; }
.check {
  background: var(--surface-2);
  border: 1px solid var(--border);
  border-radius: 8px;
  overflow: hidden;
}
.check summary {
  list-style: none;
  display: flex;
  gap: 12px;
  align-items: center;
  padding: 12px 16px;
  cursor: pointer;
}
.check summary::-webkit-details-marker { display: none; }
.check[open] summary { border-bottom: 1px solid var(--border); }
.check-name { font-weight: 600; min-width: 180px; }
.check-summary { color: var(--text-dim); flex: 1; }
.check-body { padding: 16px; }
.check-body h4 { margin: 12px 0 6px; font-size: 12px; text-transform: uppercase; letter-spacing: 0.05em; color: var(--text-dim); }
.check-body h4:first-child { margin-top: 0; }
.affected { margin: 0; padding-left: 18px; font-family: ui-monospace, monospace; font-size: 12px; color: var(--text-dim); }
.affected li { word-break: break-all; }
.check-body pre { background: var(--bg); border: 1px solid var(--border); border-radius: 6px; padding: 8px 10px; font-size: 12px; overflow: auto; white-space: pre-wrap; }

.badge {
  display: inline-block;
  padding: 2px 8px;
  border-radius: 4px;
  font-size: 11px;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 0.04em;
}
.badge--passed { background: rgba(78, 211, 145, 0.15); color: var(--pass); }
.badge--warning { background: rgba(245, 165, 36, 0.15); color: var(--warn); }
.badge--errored { background: rgba(239, 68, 68, 0.15); color: var(--err); }

.history h2 { margin: 0 0 16px; }
.search {
  width: 100%;
  padding: 10px 14px;
  background: var(--surface);
  color: var(--text);
  border: 1px solid var(--border);
  border-radius: 8px;
  margin-bottom: 16px;
  font-size: 14px;
}
.history-list { display: flex; flex-direction: column; gap: 6px; }
.history-row {
  background: var(--surface);
  border: 1px solid var(--border);
  border-radius: 8px;
  padding: 12px 16px;
  text-align: left;
  display: flex;
  justify-content: space-between;
  align-items: center;
  gap: 16px;
  color: inherit;
}
.history-row:hover { background: var(--surface-2); border-color: var(--accent); }
.history-domain { font-weight: 600; }
.history-url { font-size: 12px; color: var(--text-dim); word-break: break-all; }
.history-meta { display: flex; gap: 16px; align-items: center; flex-shrink: 0; }
.history-age { font-size: 12px; color: var(--text-dim); }
.history-score {
  font-weight: 700;
  padding: 4px 10px;
  border-radius: 6px;
  background: var(--surface-2);
  border: 1px solid var(--border);
}
.score-great { border-color: var(--pass); color: var(--pass); }
.score-ok { border-color: #fbbf24; color: #fbbf24; }
.score-warn { border-color: var(--warn); color: var(--warn); }
.score-bad { border-color: var(--err); color: var(--err); }
.history-counts { display: flex; gap: 6px; font-size: 11px; }
.history-counts .p { color: var(--pass); }
.history-counts .w { color: var(--warn); }
.history-counts .e { color: var(--err); }
.empty { color: var(--text-dim); }

.site { display: flex; flex-direction: column; gap: 24px; }
.site-head {
  display: flex;
  flex-wrap: wrap;
  gap: 24px;
  align-items: flex-end;
  justify-content: space-between;
  padding: 24px 0 8px;
}
.site-host {
  font-size: 32px;
  margin: 0;
  letter-spacing: -0.02em;
  word-break: break-all;
}
.site-sub { color: var(--text-dim); margin: 4px 0 0; font-size: 14px; }
.site-head .url-form { max-width: 420px; flex: 1 1 320px; }

.score-chart {
  background: var(--surface);
  border: 1px solid var(--border);
  border-radius: 12px;
  padding: 20px 24px 16px;
}
.score-chart h2 {
  margin: 0 0 12px;
  font-size: 13px;
  text-transform: uppercase;
  letter-spacing: 0.06em;
  color: var(--text-dim);
  font-weight: 600;
}
.chart-svg { width: 100%; height: auto; max-height: 280px; display: block; }
.chart-grid { stroke: var(--border); stroke-width: 1; stroke-dasharray: 2 4; }
.chart-axis { stroke: var(--border); stroke-width: 1; }
.chart-axis-label {
  fill: var(--text-dim);
  font-size: 11px;
  font-family: ui-monospace, SFMono-Regular, Menlo, monospace;
}
.chart-line { stroke: var(--accent); stroke-width: 2; stroke-linejoin: round; fill: none; }
.chart-dot { cursor: pointer; }
.chart-dot-circle {
  fill: var(--bg);
  stroke: var(--accent);
  stroke-width: 2;
  transition: r 120ms ease;
}
.chart-dot:hover .chart-dot-circle { r: 7; }
.chart-dot--great .chart-dot-circle { stroke: var(--pass); }
.chart-dot--ok    .chart-dot-circle { stroke: #fbbf24; }
.chart-dot--warn  .chart-dot-circle { stroke: var(--warn); }
.chart-dot--bad   .chart-dot-circle { stroke: var(--err); }
.chart-tooltip {
  opacity: 0;
  pointer-events: none;
  transition: opacity 120ms ease;
}
.chart-dot:hover .chart-tooltip { opacity: 1; }
.chart-tooltip-bg {
  fill: var(--surface-2);
  stroke: var(--border);
  stroke-width: 1;
}
.chart-tooltip-text {
  fill: var(--text);
  font-family: ui-monospace, SFMono-Regular, Menlo, monospace;
  font-size: 11px;
}

.site-history h2 {
  margin: 0 0 12px;
  font-size: 13px;
  text-transform: uppercase;
  letter-spacing: 0.06em;
  color: var(--text-dim);
  font-weight: 600;
}

.api-section {
  margin-top: 56px;
  padding: 28px 32px 32px;
  background: var(--surface);
  border: 1px solid var(--border);
  border-radius: 12px;
}
.api-section h2 {
  margin: 0 0 8px;
  font-size: 22px;
  letter-spacing: -0.01em;
}
.api-lead { color: var(--text-dim); margin: 0 0 20px; }
.api-section h3 {
  margin: 24px 0 8px;
  font-size: 12px;
  text-transform: uppercase;
  letter-spacing: 0.06em;
  color: var(--text-dim);
  font-weight: 600;
}
.api-section p { color: var(--text-dim); margin: 8px 0; }
.api-section ul { margin: 8px 0 0; padding-left: 18px; color: var(--text-dim); }
.api-section li { margin-bottom: 6px; line-height: 1.55; }
.api-section li code,
.api-section p code,
.api-tips code {
  font-family: ui-monospace, SFMono-Regular, Menlo, monospace;
  font-size: 12px;
  background: var(--bg);
  border: 1px solid var(--border);
  border-radius: 4px;
  padding: 1px 6px;
  color: var(--text);
}
.api-code {
  background: var(--bg);
  border: 1px solid var(--border);
  border-radius: 8px;
  padding: 12px 14px;
  margin: 8px 0;
  overflow-x: auto;
  white-space: pre;
}
.api-code code {
  font-family: ui-monospace, SFMono-Regular, Menlo, monospace;
  font-size: 12.5px;
  line-height: 1.55;
  color: var(--text);
  white-space: pre;
}
.api-tips { margin-top: 28px; }

.site-footer {
  text-align: center;
  padding: 20px;
  border-top: 1px solid var(--border);
  color: var(--text-dim);
  font-size: 12px;
}
"""
