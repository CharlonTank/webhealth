module JsonEncoders exposing (encodeReport)

import Json.Encode as E
import Time
import Types exposing (..)


encodeReport : AuditReport -> E.Value
encodeReport r =
    E.object
        [ ( "url", E.string r.url )
        , ( "finalUrl", E.string r.finalUrl )
        , ( "scannedAt", E.int (Time.posixToMillis r.scannedAt) )
        , ( "perceivedLoadMs", E.int r.perceivedLoadMs )
        , ( "totalTestMs", E.int r.totalTestMs )
        , ( "score", E.int r.score )
        , ( "passed", E.int r.passed )
        , ( "warnings", E.int r.warnings )
        , ( "errors", E.int r.errors )
        , ( "categories", E.list encodeCategory r.categories )
        ]


encodeCategory : Category -> E.Value
encodeCategory c =
    E.object
        [ ( "name", E.string c.name )
        , ( "checks", E.list encodeCheck c.checks )
        ]


encodeCheck : Check -> E.Value
encodeCheck c =
    E.object
        [ ( "id", E.string c.id )
        , ( "name", E.string c.name )
        , ( "severity", E.string (severityString c.severity) )
        , ( "summary", E.string c.summary )
        , ( "affectedResources", E.list E.string c.affectedResources )
        , ( "howToFix"
          , case c.howToFix of
                Just s ->
                    E.string s

                Nothing ->
                    E.null
          )
        , ( "extra"
          , E.list
                (\( k, v ) ->
                    E.object [ ( "label", E.string k ), ( "value", E.string v ) ]
                )
                c.extra
          )
        ]


severityString : Severity -> String
severityString s =
    case s of
        Pass ->
            "pass"

        Warning ->
            "warning"

        Errored ->
            "error"
