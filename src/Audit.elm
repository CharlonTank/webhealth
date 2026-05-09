module Audit exposing (buildReport, parseLinks)

import Dict exposing (Dict)
import Html.Parser exposing (Node(..))
import HtmlQuery as HQ
import Regex exposing (Regex)
import Set
import Time
import Types exposing (..)
import Url


buildReport : Time.Posix -> InflightAudit -> AuditReport
buildReport now inflight =
    let
        body =
            inflight.htmlBody |> Maybe.withDefault ""

        headers =
            inflight.htmlHeaders |> Maybe.withDefault []

        finalUrl =
            inflight.finalUrl |> Maybe.withDefault inflight.url

        nodes =
            HQ.parse body

        ctx =
            { url = inflight.url
            , finalUrl = finalUrl
            , htmlStatus = inflight.htmlStatus |> Maybe.withDefault 0
            , body = body
            , bodyLower = String.toLower body
            , headers = normalizeHeaders headers
            , nodes = nodes
            , htmlMillis = inflight.htmlMillis |> Maybe.withDefault 0
            , robots = inflight.robots
            , sitemap = inflight.sitemap
            , favicon = inflight.favicon
            , externalLinks = inflight.externalLinks
            , internalLinks = inflight.internalLinks
            }

        categories =
            [ metaCategory ctx
            , contentCategory ctx
            , technicalCategory ctx
            , accessibilityCategory ctx
            , socialCategory ctx
            , linksCategory ctx
            ]

        allChecks =
            List.concatMap .checks categories

        countBy s =
            allChecks |> List.filter (\c -> c.severity == s) |> List.length

        passed =
            countBy Pass

        warnings =
            countBy Warning

        errors =
            countBy Errored

        total =
            passed + warnings + errors

        score =
            if total == 0 then
                0

            else
                round (100 * toFloat (passed * 2 + warnings) / toFloat (total * 2))

        totalMs =
            Time.posixToMillis now - Time.posixToMillis inflight.started
    in
    { url = inflight.url
    , finalUrl = finalUrl
    , scannedAt = now
    , perceivedLoadMs = inflight.htmlMillis |> Maybe.withDefault 0
    , totalTestMs = totalMs
    , score = score
    , passed = passed
    , warnings = warnings
    , errors = errors
    , categories = categories
    }


type alias Ctx =
    { url : String
    , finalUrl : String
    , htmlStatus : Int
    , body : String
    , bodyLower : String
    , headers : List ( String, String )
    , nodes : List Node
    , htmlMillis : Int
    , robots : Maybe ProbeResult
    , sitemap : Maybe ProbeResult
    , favicon : Maybe ProbeResult
    , externalLinks : Dict String (Maybe Int)
    , internalLinks : Dict String (Maybe Int)
    }


normalizeHeaders : List ( String, String ) -> List ( String, String )
normalizeHeaders =
    List.map (\( k, v ) -> ( String.toLower k, v ))


header : String -> Ctx -> Maybe String
header name ctx =
    ctx.headers
        |> List.filter (\( k, _ ) -> k == String.toLower name)
        |> List.head
        |> Maybe.map Tuple.second


hasTag : String -> Node -> Bool
hasTag t n =
    HQ.tagName n == Just (String.toLower t)



-- ────────────────────────────────────────────────────────────────────────────
--  CATEGORY: DIAGNOSTICS (debug)
-- ────────────────────────────────────────────────────────────────────────────


diagnosticsCategory : Ctx -> Category
diagnosticsCategory ctx =
    let
        bodyLen =
            String.length ctx.body

        topNodes =
            List.length ctx.nodes

        allElems =
            ctx.nodes |> HQ.allElements |> List.length

        firstTags =
            ctx.nodes
                |> HQ.allElements
                |> List.take 12
                |> List.filterMap HQ.tagName
                |> String.join ","

        bodySnippet =
            String.left 240 ctx.body

        runDocResult =
            case Html.Parser.runDocument ctx.body of
                Ok _ ->
                    "ok"

                Err errs ->
                    "fail (" ++ String.fromInt (List.length errs) ++ " deadEnds)"

        runResult =
            case Html.Parser.run ctx.body of
                Ok ns ->
                    "ok (" ++ String.fromInt (List.length ns) ++ " top nodes)"

                Err errs ->
                    "fail (" ++ String.fromInt (List.length errs) ++ " deadEnds)"

        stripped =
            HQ.stripScriptsAndStyles ctx.body

        strippedRun =
            case Html.Parser.run stripped of
                Ok ns ->
                    "ok (" ++ String.fromInt (List.length ns) ++ ")"

                Err errs ->
                    "fail (" ++ String.fromInt (List.length errs) ++ " deadEnds)"

        firstDeadEnd =
            case Html.Parser.run stripped of
                Ok _ ->
                    "n/a"

                Err errs ->
                    case List.head errs of
                        Just de ->
                            "row=" ++ String.fromInt de.row ++ " col=" ++ String.fromInt de.col

                        Nothing ->
                            "n/a"
    in
    { name = "Diagnostics"
    , checks =
        [ { id = "diag-body"
          , name = "HTML body length"
          , severity = Pass
          , summary = String.fromInt bodyLen ++ " chars"
          , affectedResources = [ String.left 200 bodySnippet ]
          , howToFix = Nothing
          , extra = []
          }
        , { id = "diag-parse"
          , name = "Parser results"
          , severity = Pass
          , summary = "runDocument: " ++ runDocResult ++ " · run: " ++ runResult ++ " · stripped+run: " ++ strippedRun ++ " · stripped len: " ++ String.fromInt (String.length stripped) ++ " · firstErr: " ++ firstDeadEnd
          , affectedResources = []
          , howToFix = Nothing
          , extra = []
          }
        , { id = "diag-tree"
          , name = "Parsed tree"
          , severity = Pass
          , summary = String.fromInt topNodes ++ " top nodes / " ++ String.fromInt allElems ++ " elements"
          , affectedResources = [ "first tags: " ++ firstTags ]
          , howToFix = Nothing
          , extra = []
          }
        ]
    }



metaListCategory : Ctx -> Category
metaListCategory ctx =
    let
        metaNames =
            HQ.findAll (hasTag "meta") ctx.nodes
                |> List.map
                    (\n ->
                        let
                            label =
                                HQ.attr "name" n
                                    |> orElse (HQ.attr "property" n)
                                    |> Maybe.withDefault "(no name/property)"

                            content =
                                HQ.attr "content" n |> Maybe.withDefault ""
                        in
                        label ++ " = " ++ String.left 60 content
                    )
    in
    { name = "Meta tags found"
    , checks =
        [ { id = "meta-list"
          , name = "All meta tags"
          , severity = Pass
          , summary = String.fromInt (List.length metaNames) ++ " meta tags parsed"
          , affectedResources = metaNames
          , howToFix = Nothing
          , extra = []
          }
        ]
    }



-- ────────────────────────────────────────────────────────────────────────────
--  CATEGORY: META INFORMATION
-- ────────────────────────────────────────────────────────────────────────────


metaCategory : Ctx -> Category
metaCategory ctx =
    { name = "Meta Information"
    , checks =
        [ titleCheck ctx
        , metaDescriptionCheck ctx
        , canonicalCheck ctx
        , faviconCheck ctx
        , viewportCheck ctx
        , htmlLangCheck ctx
        ]
    }


titleCheck : Ctx -> Check
titleCheck ctx =
    let
        title =
            HQ.findFirst (hasTag "title") ctx.nodes
                |> Maybe.map (HQ.textOf >> String.trim)

        len =
            title |> Maybe.map String.length |> Maybe.withDefault 0
    in
    case title of
        Nothing ->
            { id = "title-tag"
            , name = "Title Tag"
            , severity = Errored
            , summary = "No <title> element found."
            , affectedResources = []
            , howToFix = Just "Add a descriptive <title> in <head>, ideally 30-65 characters."
            , extra = []
            }

        Just _ ->
            if len < 30 then
                check "title-tag" "Title Tag" Warning ("Found " ++ String.fromInt len ++ " characters. Title is shorter than recommended (30-65).") (Just "Expand the title to 30-65 characters with the primary keyword.")

            else if len > 65 then
                check "title-tag" "Title Tag" Warning ("Found " ++ String.fromInt len ++ " characters. Title may be truncated in SERPs.") (Just "Trim the title to under 65 characters.")

            else
                check "title-tag" "Title Tag" Pass ("Found " ++ String.fromInt len ++ " characters. Length is optimal.") Nothing


metaDescriptionCheck : Ctx -> Check
metaDescriptionCheck ctx =
    let
        desc =
            findMeta "description" ctx.nodes

        len =
            desc |> Maybe.map String.length |> Maybe.withDefault 0
    in
    case desc of
        Nothing ->
            { id = "meta-description"
            , name = "Meta Description"
            , severity = Warning
            , summary = "No meta description found."
            , affectedResources = []
            , howToFix = Just "Add <meta name=\"description\" content=\"...\"> with 110-160 characters."
            , extra = []
            }

        Just _ ->
            if len < 50 then
                check "meta-description" "Meta Description" Warning ("Found " ++ String.fromInt len ++ " characters. Snippet may be too short.") (Just "Expand to 110-160 characters.")

            else if len > 200 then
                check "meta-description" "Meta Description" Warning ("Found " ++ String.fromInt len ++ " characters. Snippet may be truncated.") (Just "Trim to 110-160 characters.")

            else
                check "meta-description" "Meta Description" Pass ("Found " ++ String.fromInt len ++ " characters. Good snippet length.") Nothing


canonicalCheck : Ctx -> Check
canonicalCheck ctx =
    let
        canonical =
            HQ.findAll (hasTag "link") ctx.nodes
                |> List.filter (\n -> HQ.attr "rel" n |> Maybe.map String.toLower |> (==) (Just "canonical"))
                |> List.head
                |> Maybe.andThen (HQ.attr "href")
    in
    case canonical of
        Nothing ->
            check "canonical" "Canonical URL" Warning "No canonical link tag found." (Just "Add <link rel=\"canonical\" href=\"...\"> to declare the preferred URL.")

        Just c ->
            check "canonical" "Canonical URL" Pass ("Canonical found: " ++ c) Nothing


faviconCheck : Ctx -> Check
faviconCheck ctx =
    let
        href =
            HQ.findAll (hasTag "link") ctx.nodes
                |> List.filter
                    (\n ->
                        case HQ.attr "rel" n |> Maybe.map String.toLower of
                            Just rel ->
                                String.contains "icon" rel

                            Nothing ->
                                False
                    )
                |> List.head
                |> Maybe.andThen (HQ.attr "href")
    in
    case ( href, ctx.favicon ) of
        ( Nothing, _ ) ->
            check "favicon" "Favicon" Warning "No <link rel=\"icon\"> declared. Browsers will fall back to /favicon.ico." (Just "Declare a favicon explicitly with <link rel=\"icon\" href=\"...\">")

        ( Just h, Just probe ) ->
            if probe.status >= 200 && probe.status < 400 then
                check "favicon" "Favicon" Pass ("Favicon found and reachable: " ++ h ++ " (HTTP " ++ String.fromInt probe.status ++ ").") Nothing

            else
                check "favicon" "Favicon" Warning ("Favicon declared but returned HTTP " ++ String.fromInt probe.status ++ ".") (Just "Make the favicon URL reachable with HTTP 200.")

        ( Just h, Nothing ) ->
            check "favicon" "Favicon" Warning ("Favicon declared (" ++ h ++ ") but probe was inconclusive.") Nothing


viewportCheck : Ctx -> Check
viewportCheck ctx =
    case findMeta "viewport" ctx.nodes of
        Nothing ->
            check "viewport" "Viewport Meta" Errored "No viewport meta declared." (Just "Add <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">.")

        Just v ->
            check "viewport" "Viewport Meta" Pass ("Viewport configured: " ++ v) Nothing


htmlLangCheck : Ctx -> Check
htmlLangCheck ctx =
    let
        lang =
            HQ.findFirst (hasTag "html") ctx.nodes
                |> Maybe.andThen (HQ.attr "lang")
    in
    case lang of
        Nothing ->
            check "html-lang" "HTML Lang" Warning "No lang attribute on <html>." (Just "Set <html lang=\"en\"> (or appropriate language code).")

        Just l ->
            check "html-lang" "HTML Lang" Pass ("Language declared as \"" ++ l ++ "\".") Nothing



-- ────────────────────────────────────────────────────────────────────────────
--  CATEGORY: CONTENT STRUCTURE
-- ────────────────────────────────────────────────────────────────────────────


contentCategory : Ctx -> Category
contentCategory ctx =
    { name = "Content Structure"
    , checks =
        [ h1Check ctx
        , headingHierarchyCheck ctx
        , imageAltCheck ctx
        ]
    }


h1Check : Ctx -> Check
h1Check ctx =
    let
        h1s =
            HQ.findAll (hasTag "h1") ctx.nodes
                |> List.map (HQ.textOf >> String.trim)
                |> List.filter (not << String.isEmpty)
    in
    case h1s of
        [] ->
            check "h1" "H1 Tag" Errored "No <h1> found on the page." (Just "Add a single descriptive <h1> as the page heading.")

        [ one ] ->
            check "h1" "H1 Tag" Pass ("Exactly one H1 found: \"" ++ truncate 80 one ++ "\".") Nothing

        many ->
            { id = "h1"
            , name = "H1 Tag"
            , severity = Warning
            , summary = String.fromInt (List.length many) ++ " H1 elements found. Pages should have a single primary heading."
            , affectedResources = List.map (truncate 80) many
            , howToFix = Just "Keep one <h1> per page; use <h2>-<h6> for sub-sections."
            , extra = []
            }


headingHierarchyCheck : Ctx -> Check
headingHierarchyCheck ctx =
    let
        levels =
            HQ.allElements ctx.nodes
                |> List.filterMap headingLevel

        breaks =
            findHierarchyBreaks levels
    in
    case ( levels, breaks ) of
        ( [], _ ) ->
            check "heading-hierarchy" "Heading Hierarchy" Warning "No headings detected." (Just "Use headings to structure content.")

        ( _, [] ) ->
            check "heading-hierarchy" "Heading Hierarchy" Pass ("Valid heading flow across " ++ String.fromInt (List.length levels) ++ " headings.") Nothing

        ( _, bs ) ->
            { id = "heading-hierarchy"
            , name = "Heading Hierarchy"
            , severity = Warning
            , summary = String.fromInt (List.length bs) ++ " hierarchy jumps detected (e.g. h1 → h3)."
            , affectedResources = List.map (\( a, b ) -> "h" ++ String.fromInt a ++ " → h" ++ String.fromInt b) bs
            , howToFix = Just "Avoid skipping heading levels; nest h2 under h1, h3 under h2, etc."
            , extra = []
            }


headingLevel : Node -> Maybe Int
headingLevel n =
    case HQ.tagName n of
        Just "h1" ->
            Just 1

        Just "h2" ->
            Just 2

        Just "h3" ->
            Just 3

        Just "h4" ->
            Just 4

        Just "h5" ->
            Just 5

        Just "h6" ->
            Just 6

        _ ->
            Nothing


findHierarchyBreaks : List Int -> List ( Int, Int )
findHierarchyBreaks levels =
    case levels of
        a :: b :: rest ->
            if b > a + 1 then
                ( a, b ) :: findHierarchyBreaks (b :: rest)

            else
                findHierarchyBreaks (b :: rest)

        _ ->
            []


imageAltCheck : Ctx -> Check
imageAltCheck ctx =
    let
        imgs =
            HQ.findAll (hasTag "img") ctx.nodes

        missing =
            imgs
                |> List.filter
                    (\n ->
                        case HQ.attr "alt" n of
                            Nothing ->
                                True

                            Just _ ->
                                False
                    )
                |> List.filterMap (HQ.attr "src")
    in
    if List.isEmpty imgs then
        check "img-alt" "Image Alt Text" Pass "No <img> elements present." Nothing

    else if List.isEmpty missing then
        check "img-alt" "Image Alt Text" Pass ("All " ++ String.fromInt (List.length imgs) ++ " images include alt text.") Nothing

    else
        { id = "img-alt"
        , name = "Image Alt Text"
        , severity = Warning
        , summary = String.fromInt (List.length missing) ++ " of " ++ String.fromInt (List.length imgs) ++ " images are missing alt text."
        , affectedResources = List.take 10 missing
        , howToFix = Just "Add a descriptive alt attribute to every <img>; use alt=\"\" for purely decorative images."
        , extra = []
        }



-- ────────────────────────────────────────────────────────────────────────────
--  CATEGORY: TECHNICAL OPTIMIZATION
-- ────────────────────────────────────────────────────────────────────────────


technicalCategory : Ctx -> Category
technicalCategory ctx =
    { name = "Technical Optimization"
    , checks =
        [ httpsCheck ctx
        , hstsCheck ctx
        , securityHeadersCheck ctx
        , cspCheck ctx
        , cookieSecurityCheck ctx
        , serverDisclosureCheck ctx
        , cloudflareCheck ctx
        , perceivedLoadCheck ctx
        , renderBlockingCheck ctx
        , compressionCheck ctx
        , robotsCheck ctx
        , sitemapCheck ctx
        , crawlDirectivesCheck ctx
        ]
    }


httpsCheck : Ctx -> Check
httpsCheck ctx =
    if String.startsWith "https://" ctx.finalUrl then
        check "https" "HTTPS" Pass "Page is served over HTTPS." Nothing

    else
        check "https" "HTTPS" Errored "Page is not served over HTTPS." (Just "Obtain a TLS certificate (e.g. Let's Encrypt) and serve all traffic over HTTPS.")


hstsCheck : Ctx -> Check
hstsCheck ctx =
    case header "strict-transport-security" ctx of
        Nothing ->
            { id = "hsts"
            , name = "HSTS & HTTPS Redirect"
            , severity = Warning
            , summary = "No Strict-Transport-Security header set."
            , affectedResources = []
            , howToFix = Just "Add Strict-Transport-Security with a long max-age, includeSubDomains, and preload."
            , extra = []
            }

        Just hsts ->
            let
                hasMaxAge =
                    String.contains "max-age" hsts

                hasIncludeSub =
                    String.contains "includeSubDomains" hsts
            in
            if hasMaxAge && hasIncludeSub then
                { id = "hsts"
                , name = "HSTS & HTTPS Redirect"
                , severity = Pass
                , summary = "Strict-Transport-Security is configured."
                , affectedResources = [ "Strict-Transport-Security: " ++ hsts ]
                , howToFix = Nothing
                , extra = []
                }

            else
                { id = "hsts"
                , name = "HSTS & HTTPS Redirect"
                , severity = Warning
                , summary = "HSTS is set but missing recommended directives."
                , affectedResources = [ "Strict-Transport-Security: " ++ hsts ]
                , howToFix = Just "Set Strict-Transport-Security with max-age >= 31536000, includeSubDomains, and preload."
                , extra = []
                }


securityHeadersCheck : Ctx -> Check
securityHeadersCheck ctx =
    let
        required =
            [ "content-security-policy"
            , "x-content-type-options"
            , "x-frame-options"
            , "referrer-policy"
            , "permissions-policy"
            ]

        missing =
            required
                |> List.filter (\h -> header h ctx == Nothing)

        all =
            ctx.headers
                |> List.map (\( k, v ) -> k ++ ": " ++ v)
    in
    if List.isEmpty missing then
        { id = "security-headers"
        , name = "Security Headers"
        , severity = Pass
        , summary = "All recommended security headers are set."
        , affectedResources = []
        , howToFix = Nothing
        , extra = [ ( "Full HTTP headers (" ++ String.fromInt (List.length ctx.headers) ++ ")", String.join "\n" all ) ]
        }

    else
        { id = "security-headers"
        , name = "Security Headers"
        , severity = Warning
        , summary = "Missing: " ++ String.join ", " missing ++ "."
        , affectedResources = all
        , howToFix = Just "Add the missing security headers at your reverse proxy or application layer."
        , extra = []
        }


cspCheck : Ctx -> Check
cspCheck ctx =
    case header "content-security-policy" ctx of
        Nothing ->
            { id = "csp-quality"
            , name = "CSP Quality"
            , severity = Warning
            , summary = "Content-Security-Policy header is missing."
            , affectedResources = [ "Missing Content-Security-Policy header." ]
            , howToFix = Just "Define a restrictive Content-Security-Policy and avoid unsafe directives such as unsafe-inline and unsafe-eval."
            , extra = []
            }

        Just csp ->
            let
                unsafeDirectives =
                    [ "unsafe-inline", "unsafe-eval", "*" ]
                        |> List.filter (\d -> String.contains d csp)
            in
            if List.isEmpty unsafeDirectives then
                check "csp-quality" "CSP Quality" Pass "Content-Security-Policy is set with no obvious unsafe directives." Nothing

            else
                { id = "csp-quality"
                , name = "CSP Quality"
                , severity = Warning
                , summary = "CSP includes potentially unsafe directives: " ++ String.join ", " unsafeDirectives ++ "."
                , affectedResources = [ csp ]
                , howToFix = Just "Tighten the policy: avoid unsafe-inline, unsafe-eval, and wildcard sources."
                , extra = []
                }


cookieSecurityCheck : Ctx -> Check
cookieSecurityCheck ctx =
    let
        cookies =
            ctx.headers
                |> List.filter (\( k, _ ) -> k == "set-cookie")
                |> List.map Tuple.second
    in
    if List.isEmpty cookies then
        check "cookies" "Cookie Security" Pass "No first-party cookies were set during the initial page load." Nothing

    else
        let
            insecure =
                cookies
                    |> List.filter
                        (\c ->
                            not (String.contains "Secure" c)
                                || not (String.contains "HttpOnly" c)
                                || not (String.contains "SameSite" c)
                        )
        in
        if List.isEmpty insecure then
            check "cookies" "Cookie Security" Pass (String.fromInt (List.length cookies) ++ " cookies with secure flags.") Nothing

        else
            { id = "cookies"
            , name = "Cookie Security"
            , severity = Warning
            , summary = String.fromInt (List.length insecure) ++ " cookies missing Secure/HttpOnly/SameSite flags."
            , affectedResources = List.map (truncate 120) insecure
            , howToFix = Just "Add Secure, HttpOnly, and SameSite=Lax (or Strict) flags to cookies."
            , extra = []
            }


serverDisclosureCheck : Ctx -> Check
serverDisclosureCheck ctx =
    let
        suspicious =
            ctx.headers
                |> List.filter
                    (\( k, v ) ->
                        (k == "server" || k == "x-powered-by")
                            && Regex.contains versionRegex v
                    )
                |> List.map (\( k, v ) -> k ++ ": " ++ v)
    in
    if List.isEmpty suspicious then
        check "server-version" "Server Version Disclosure" Pass "Server response headers do not expose version tokens." Nothing

    else
        { id = "server-version"
        , name = "Server Version Disclosure"
        , severity = Warning
        , summary = "Server/X-Powered-By headers reveal version numbers."
        , affectedResources = suspicious
        , howToFix = Just "Strip version tokens from Server and X-Powered-By headers."
        , extra = []
        }


versionRegex : Regex
versionRegex =
    Regex.fromString "[0-9]+\\.[0-9]+"
        |> Maybe.withDefault Regex.never


cloudflareCheck : Ctx -> Check
cloudflareCheck ctx =
    let
        cfHeaders =
            ctx.headers
                |> List.filter (\( k, _ ) -> String.startsWith "cf-" k || k == "server")
                |> List.map (\( k, v ) -> k ++ ": " ++ v)

        isCf =
            (header "server" ctx |> Maybe.map (String.contains "cloudflare") |> Maybe.withDefault False)
                || (header "cf-ray" ctx /= Nothing)
    in
    if isCf then
        { id = "cloudflare"
        , name = "Cloudflare Proxy"
        , severity = Pass
        , summary = "Domain appears to be behind Cloudflare."
        , affectedResources = cfHeaders
        , howToFix = Nothing
        , extra = []
        }

    else
        check "cloudflare" "Cloudflare Proxy" Pass "No Cloudflare proxy detected (informational)." Nothing


perceivedLoadCheck : Ctx -> Check
perceivedLoadCheck ctx =
    let
        s =
            toFloat ctx.htmlMillis / 1000
    in
    if ctx.htmlMillis == 0 then
        check "perceived-load" "Perceived Load Time" Warning "Could not measure load time." Nothing

    else if s < 1.5 then
        check "perceived-load" "Perceived Load Time" Pass ("Loaded in " ++ formatSeconds s ++ " (perceived).") Nothing

    else if s < 3.5 then
        check "perceived-load" "Perceived Load Time" Warning ("Loaded in " ++ formatSeconds s ++ ". Aim for under 1.5s.") (Just "Reduce TTFB, enable compression, and optimize critical render path.")

    else
        check "perceived-load" "Perceived Load Time" Errored ("Loaded in " ++ formatSeconds s ++ ". This is well above recommendations.") (Just "Investigate slow TTFB, large payloads, and render-blocking resources.")


renderBlockingCheck : Ctx -> Check
renderBlockingCheck ctx =
    let
        styles =
            HQ.findAll (hasTag "link") ctx.nodes
                |> List.filter (\n -> HQ.attr "rel" n |> Maybe.map String.toLower |> (==) (Just "stylesheet"))
                |> List.filterMap (HQ.attr "href")

        blockingScripts =
            HQ.findAll (hasTag "script") ctx.nodes
                |> List.filter
                    (\n ->
                        case HQ.attr "src" n of
                            Just _ ->
                                HQ.attr "async" n == Nothing && HQ.attr "defer" n == Nothing

                            Nothing ->
                                False
                    )
                |> List.filterMap (HQ.attr "src")

        affected =
            List.map (\s -> "style: " ++ s) styles
                ++ List.map (\s -> "script: " ++ s) blockingScripts

        sev =
            if List.length affected == 0 then
                Pass

            else
                Warning
    in
    { id = "render-blocking"
    , name = "Render Blocking Resources"
    , severity = sev
    , summary = String.fromInt (List.length blockingScripts) ++ " scripts and " ++ String.fromInt (List.length styles) ++ " styles may block rendering."
    , affectedResources = affected
    , howToFix =
        if sev == Pass then
            Nothing

        else
            Just "Defer non-critical scripts and inline critical CSS to improve first paint speed."
    , extra = []
    }


compressionCheck : Ctx -> Check
compressionCheck ctx =
    case header "content-encoding" ctx of
        Just enc ->
            check "compression" "Compression" Pass ("Text-like assets appear compressed (" ++ enc ++ ").") Nothing

        Nothing ->
            check "compression" "Compression" Warning "No Content-Encoding header. Page may not be compressed." (Just "Enable Brotli or gzip on text/HTML/JS/CSS responses.")


robotsCheck : Ctx -> Check
robotsCheck ctx =
    case ctx.robots of
        Just probe ->
            if probe.status >= 200 && probe.status < 400 then
                check "robots" "Robots.txt" Pass ("Found robots.txt (" ++ String.fromInt probe.status ++ ").") Nothing

            else
                check "robots" "Robots.txt" Warning ("robots.txt returned HTTP " ++ String.fromInt probe.status ++ ".") (Just "Serve a robots.txt at the site root.")

        Nothing ->
            check "robots" "Robots.txt" Warning "Could not probe robots.txt." Nothing


sitemapCheck : Ctx -> Check
sitemapCheck ctx =
    case ctx.sitemap of
        Just probe ->
            if probe.status >= 200 && probe.status < 400 then
                check "sitemap" "Sitemap File" Pass ("Found sitemap (" ++ String.fromInt probe.status ++ ") at " ++ probe.url ++ ".") Nothing

            else
                check "sitemap" "Sitemap File" Warning ("Sitemap returned HTTP " ++ String.fromInt probe.status ++ ".") (Just "Generate a /sitemap.xml referenced from robots.txt.")

        Nothing ->
            check "sitemap" "Sitemap File" Warning "Could not probe sitemap.xml." Nothing


crawlDirectivesCheck : Ctx -> Check
crawlDirectivesCheck ctx =
    case findMeta "robots" ctx.nodes of
        Just v ->
            let
                blocked =
                    String.contains "noindex" (String.toLower v)
            in
            if blocked then
                check "crawl-directives" "Crawl Directives" Errored ("Robots meta directs noindex: " ++ v) (Just "Remove noindex if you want this page indexed.")

            else
                check "crawl-directives" "Crawl Directives" Pass ("Robots meta found: " ++ v) Nothing

        Nothing ->
            check "crawl-directives" "Crawl Directives" Pass "No robots meta restricting crawling." Nothing



-- ────────────────────────────────────────────────────────────────────────────
--  CATEGORY: ACCESSIBILITY
-- ────────────────────────────────────────────────────────────────────────────


accessibilityCategory : Ctx -> Category
accessibilityCategory ctx =
    { name = "Accessibility Basics"
    , checks =
        [ formLabelsCheck ctx
        , landmarksCheck ctx
        , tapTargetCheck ctx
        ]
    }


formLabelsCheck : Ctx -> Check
formLabelsCheck ctx =
    let
        controls =
            HQ.findAll (\n -> List.member (HQ.tagName n) [ Just "input", Just "select", Just "textarea" ]) ctx.nodes
                |> List.filter
                    (\n ->
                        case HQ.attr "type" n |> Maybe.map String.toLower of
                            Just "hidden" ->
                                False

                            Just "submit" ->
                                False

                            Just "button" ->
                                False

                            _ ->
                                True
                    )

        labelFors =
            HQ.findAll (hasTag "label") ctx.nodes
                |> List.filterMap (HQ.attr "for")
                |> Set.fromList

        unlabeled =
            controls
                |> List.filter
                    (\n ->
                        let
                            id =
                                HQ.attr "id" n |> Maybe.withDefault ""

                            aria =
                                HQ.attr "aria-label" n
                                    |> orElse (HQ.attr "aria-labelledby" n)
                                    |> orElse (HQ.attr "placeholder" n)
                        in
                        not (Set.member id labelFors) && aria == Nothing
                    )
    in
    if List.isEmpty controls then
        check "form-labels" "Form Labels" Pass "All 0 controls are labeled." Nothing

    else if List.isEmpty unlabeled then
        check "form-labels" "Form Labels" Pass ("All " ++ String.fromInt (List.length controls) ++ " controls are labeled.") Nothing

    else
        { id = "form-labels"
        , name = "Form Labels"
        , severity = Warning
        , summary = String.fromInt (List.length unlabeled) ++ " of " ++ String.fromInt (List.length controls) ++ " form controls have no label."
        , affectedResources =
            unlabeled
                |> List.map (\n -> Maybe.withDefault "(no name)" (HQ.attr "name" n))
                |> List.take 10
        , howToFix = Just "Associate every form control with a <label for=\"...\"> or aria-label."
        , extra = []
        }


landmarksCheck : Ctx -> Check
landmarksCheck ctx =
    let
        present tag =
            HQ.findFirst (hasTag tag) ctx.nodes /= Nothing

        missing =
            [ "header", "nav", "main", "footer" ]
                |> List.filter (not << present)
    in
    if List.isEmpty missing then
        check "landmarks" "Landmarks" Pass "Header, nav, main, and footer landmarks are present." Nothing

    else
        { id = "landmarks"
        , name = "Landmarks"
        , severity = Warning
        , summary = "Missing landmark elements: " ++ String.join ", " missing ++ "."
        , affectedResources = []
        , howToFix = Just "Wrap top-level content with <header>, <nav>, <main>, and <footer> for assistive tech."
        , extra = []
        }


tapTargetCheck : Ctx -> Check
tapTargetCheck _ =
    -- Real measurement requires layout — surface as informational.
    check "tap-targets" "Tap Target Size" Pass "Tap target measurement requires a rendered viewport (skipped in OSS build)." Nothing



-- ────────────────────────────────────────────────────────────────────────────
--  CATEGORY: SOCIAL & RICH RESULTS
-- ────────────────────────────────────────────────────────────────────────────


socialCategory : Ctx -> Category
socialCategory ctx =
    { name = "Social & Rich Results"
    , checks =
        [ ogBasicsCheck ctx
        , ogImageCheck ctx
        , twitterCardCheck ctx
        , structuredDataCheck ctx
        , pwaMetadataCheck ctx
        , ogQualityCheck ctx
        ]
    }


ogBasicsCheck : Ctx -> Check
ogBasicsCheck ctx =
    let
        required =
            [ "og:title", "og:description", "og:url", "og:type" ]

        missing =
            required
                |> List.filter (\p -> findOg p ctx.nodes == Nothing)
    in
    if List.isEmpty missing then
        check "og-basics" "Open Graph Basics" Pass "Core Open Graph tags are present." Nothing

    else
        { id = "og-basics"
        , name = "Open Graph Basics"
        , severity = Warning
        , summary = "Missing Open Graph tags: " ++ String.join ", " missing ++ "."
        , affectedResources = []
        , howToFix = Just "Add og:title, og:description, og:url, og:type, og:image meta tags."
        , extra = []
        }


ogImageCheck : Ctx -> Check
ogImageCheck ctx =
    case findOg "og:image" ctx.nodes of
        Nothing ->
            check "og-image" "Open Graph Image" Warning "No og:image declared." (Just "Add an og:image with an absolute URL near 1200x630.")

        Just url ->
            if String.startsWith "http" url then
                check "og-image" "Open Graph Image" Pass "og:image is present and absolute." Nothing

            else
                check "og-image" "Open Graph Image" Warning ("og:image is not an absolute URL: " ++ url) (Just "Use an absolute https URL for og:image.")


twitterCardCheck : Ctx -> Check
twitterCardCheck ctx =
    case findMeta "twitter:card" ctx.nodes of
        Nothing ->
            check "twitter-card" "Twitter Card" Warning "No twitter:card meta declared." (Just "Add <meta name=\"twitter:card\" content=\"summary_large_image\">.")

        Just c ->
            check "twitter-card" "Twitter Card" Pass ("twitter:card set to " ++ c ++ ".") Nothing


structuredDataCheck : Ctx -> Check
structuredDataCheck ctx =
    let
        ldjson =
            HQ.findAll (hasTag "script") ctx.nodes
                |> List.filter (\n -> HQ.attr "type" n |> Maybe.map String.toLower |> (==) (Just "application/ld+json"))
    in
    if List.isEmpty ldjson then
        check "structured-data" "Structured Data" Warning "No JSON-LD structured data detected." (Just "Add JSON-LD schema for the page (Organization, Article, Product, etc.).")

    else
        check "structured-data" "Structured Data" Pass "JSON-LD schema detected." Nothing


pwaMetadataCheck : Ctx -> Check
pwaMetadataCheck ctx =
    let
        manifest =
            HQ.findAll (hasTag "link") ctx.nodes
                |> List.any (\n -> HQ.attr "rel" n |> Maybe.map String.toLower |> (==) (Just "manifest"))

        appleIcon =
            HQ.findAll (hasTag "link") ctx.nodes
                |> List.any (\n -> HQ.attr "rel" n |> Maybe.map String.toLower |> (==) (Just "apple-touch-icon"))
    in
    if manifest && appleIcon then
        check "pwa" "PWA Metadata" Pass "Manifest and apple-touch-icon are linked." Nothing

    else
        check "pwa" "PWA Metadata" Warning "Manifest or Apple touch icon is missing." (Just "Link your web app manifest and apple-touch-icon for improved install/share experiences.")


ogQualityCheck : Ctx -> Check
ogQualityCheck ctx =
    let
        title =
            findOg "og:title" ctx.nodes |> Maybe.map String.length |> Maybe.withDefault 0

        desc =
            findOg "og:description" ctx.nodes |> Maybe.map String.length |> Maybe.withDefault 0

        twCard =
            findMeta "twitter:card" ctx.nodes |> Maybe.withDefault ""

        issues =
            List.concat
                [ if title > 0 && (title < 10 || title > 70) then
                    [ "og:title length (" ++ String.fromInt title ++ ") outside acceptable range (10-70)." ]

                  else
                    []
                , if desc > 0 && (desc < 50 || desc > 200) then
                    [ "og:description length (" ++ String.fromInt desc ++ ") outside acceptable range (50-200)." ]

                  else
                    []
                , if twCard == "summary" then
                    [ "twitter:card should be summary_large_image for richer previews." ]

                  else
                    []
                ]
    in
    if List.isEmpty issues then
        check "og-quality" "Open Graph/Twitter Quality" Pass "Social previews look healthy." Nothing

    else
        { id = "og-quality"
        , name = "Open Graph/Twitter Quality"
        , severity = Warning
        , summary = String.fromInt (List.length issues) ++ " social preview quality issues detected."
        , affectedResources = issues
        , howToFix = Just "Use absolute OG/Twitter URLs, keep metadata lengths in recommended ranges, and provide a preview image near 1200x630 under 5MB."
        , extra =
            [ ( "Guidelines"
              , String.join "\n"
                    [ "Optimal og:title length: 40-60 characters (acceptable: 10-70)."
                    , "Optimal og:description length: 110-160 characters (acceptable: 50-200)."
                    , "Optimal preview image size: 1200x630 pixels."
                    , "Recommended twitter:card: summary_large_image."
                    ]
              )
            ]
        }



-- ────────────────────────────────────────────────────────────────────────────
--  CATEGORY: LINKS
-- ────────────────────────────────────────────────────────────────────────────


linksCategory : Ctx -> Category
linksCategory ctx =
    { name = "Links Analysis"
    , checks =
        [ internalLinksCheck ctx
        , externalLinksCheck ctx
        , linkFormatCheck ctx
        ]
    }


internalLinksCheck : Ctx -> Check
internalLinksCheck ctx =
    summarizeLinks "internal-links" "Internal Links" ctx.internalLinks


externalLinksCheck : Ctx -> Check
externalLinksCheck ctx =
    summarizeLinks "external-links" "External Links" ctx.externalLinks


summarizeLinks : String -> String -> Dict String (Maybe Int) -> Check
summarizeLinks id name probes =
    let
        entries =
            Dict.toList probes

        broken =
            entries
                |> List.filter
                    (\( _, ms ) ->
                        case ms of
                            Just s ->
                                s >= 400

                            Nothing ->
                                True
                    )
    in
    if Dict.isEmpty probes then
        check id name Pass "No links to probe." Nothing

    else if List.isEmpty broken then
        check id name Pass ("Checked " ++ String.fromInt (Dict.size probes) ++ " links. No broken links found.") Nothing

    else
        { id = id
        , name = name
        , severity = Warning
        , summary = String.fromInt (List.length broken) ++ " links returned errors or timed out."
        , affectedResources =
            broken
                |> List.map
                    (\( u, ms ) ->
                        case ms of
                            Just s ->
                                u ++ " (HTTP " ++ String.fromInt s ++ ")"

                            Nothing ->
                                u ++ " (network-error)"
                    )
                |> List.take 20
        , howToFix = Just "Replace dead URLs or point to working alternatives."
        , extra = []
        }


linkFormatCheck : Ctx -> Check
linkFormatCheck ctx =
    let
        anchors =
            HQ.findAll (hasTag "a") ctx.nodes

        empty =
            anchors
                |> List.filter
                    (\n ->
                        case HQ.attr "href" n of
                            Just h ->
                                String.trim h == ""

                            Nothing ->
                                True
                    )
    in
    if List.isEmpty anchors then
        check "link-format" "Link Format" Pass "No anchor tags found." Nothing

    else if List.isEmpty empty then
        check "link-format" "Link Format" Pass ("All " ++ String.fromInt (List.length anchors) ++ " links use non-empty href values.") Nothing

    else
        { id = "link-format"
        , name = "Link Format"
        , severity = Warning
        , summary = String.fromInt (List.length empty) ++ " anchors have empty or missing href."
        , affectedResources = []
        , howToFix = Just "Use a valid href on every <a>, or replace with <button> when not navigating."
        , extra = []
        }



-- ────────────────────────────────────────────────────────────────────────────
--  Link extraction (for backend probes)
-- ────────────────────────────────────────────────────────────────────────────


parseLinks : String -> String -> { internal : List String, external : List String }
parseLinks pageUrl body =
    let
        host =
            Url.fromString pageUrl |> Maybe.map .host |> Maybe.withDefault ""

        nodes =
            HQ.parse body

        hrefs =
            HQ.findAll (hasTag "a") nodes
                |> List.filterMap (HQ.attr "href")
                |> List.map String.trim
                |> List.filter (\h -> h /= "" && not (String.startsWith "#" h) && not (String.startsWith "mailto:" h) && not (String.startsWith "tel:" h) && not (String.startsWith "javascript:" h))
                |> List.map (resolveUrl pageUrl)
                |> dedupe

        ( ext, int ) =
            hrefs
                |> List.partition
                    (\h ->
                        case Url.fromString h of
                            Just u ->
                                u.host /= host

                            Nothing ->
                                False
                    )
    in
    { internal = List.take 10 int, external = List.take 10 ext }


resolveUrl : String -> String -> String
resolveUrl pageUrl href =
    if String.startsWith "http://" href || String.startsWith "https://" href then
        href

    else if String.startsWith "//" href then
        case Url.fromString pageUrl of
            Just u ->
                schemeToString u.protocol ++ ":" ++ href

            Nothing ->
                href

    else if String.startsWith "/" href then
        case Url.fromString pageUrl of
            Just u ->
                schemeToString u.protocol ++ "://" ++ u.host ++ portString u.port_ ++ href

            Nothing ->
                href

    else
        case Url.fromString pageUrl of
            Just u ->
                let
                    base =
                        schemeToString u.protocol ++ "://" ++ u.host ++ portString u.port_

                    dir =
                        u.path
                            |> String.split "/"
                            |> List.reverse
                            |> List.drop 1
                            |> List.reverse
                            |> String.join "/"
                in
                base ++ dir ++ "/" ++ href

            Nothing ->
                href


schemeToString : Url.Protocol -> String
schemeToString p =
    case p of
        Url.Https ->
            "https"

        Url.Http ->
            "http"


portString : Maybe Int -> String
portString p =
    case p of
        Just n ->
            ":" ++ String.fromInt n

        Nothing ->
            ""


dedupe : List String -> List String
dedupe xs =
    Set.toList (Set.fromList xs)



-- ────────────────────────────────────────────────────────────────────────────
--  Helpers
-- ────────────────────────────────────────────────────────────────────────────


check : String -> String -> Severity -> String -> Maybe String -> Check
check id name sev summary fix =
    { id = id
    , name = name
    , severity = sev
    , summary = summary
    , affectedResources = []
    , howToFix = fix
    , extra = []
    }


findMeta : String -> List Node -> Maybe String
findMeta name nodes =
    let
        target =
            String.toLower name

        match attribute n =
            HQ.attr attribute n |> Maybe.map String.toLower |> (==) (Just target)

        metas =
            HQ.findAll (hasTag "meta") nodes
    in
    metas
        |> List.filter (\n -> match "name" n || match "property" n)
        |> List.head
        |> Maybe.andThen (HQ.attr "content")


findOg : String -> List Node -> Maybe String
findOg property nodes =
    HQ.findAll (hasTag "meta") nodes
        |> List.filter (\n -> HQ.attr "property" n |> Maybe.map String.toLower |> (==) (Just (String.toLower property)))
        |> List.head
        |> Maybe.andThen (HQ.attr "content")


truncate : Int -> String -> String
truncate n s =
    if String.length s <= n then
        s

    else
        String.left n s ++ "…"


formatSeconds : Float -> String
formatSeconds s =
    let
        rounded =
            toFloat (round (s * 100)) / 100
    in
    String.fromFloat rounded ++ "s"


orElse : Maybe a -> Maybe a -> Maybe a
orElse fallback m =
    case m of
        Just _ ->
            m

        Nothing ->
            fallback
