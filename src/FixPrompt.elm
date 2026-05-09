module FixPrompt exposing (build)

import Types exposing (..)


build : AuditReport -> List Check -> String
build report issues =
    let
        intro =
            String.join "\n"
                [ "You are reviewing the following site for launch readiness:"
                , "URL: " ++ report.finalUrl
                , "Score: " ++ String.fromInt report.score ++ "/100"
                , "Counts: " ++ String.fromInt report.passed ++ " passed, " ++ String.fromInt report.warnings ++ " warnings, " ++ String.fromInt report.errors ++ " errors."
                , ""
                , "The following issues were detected. For each, identify whether the fix belongs in first-party code (HTML, headers, server config, components) and propose a concrete remediation. Skip third-party-only items."
                , ""
                ]

        body =
            issues
                |> List.indexedMap formatIssue
                |> String.join "\n\n"

        outro =
            "\n\nReturn a numbered plan covering each issue, with file/path suggestions where applicable."
    in
    intro ++ body ++ outro


formatIssue : Int -> Check -> String
formatIssue idx c =
    let
        n =
            String.fromInt (idx + 1)

        sev =
            case c.severity of
                Pass ->
                    "PASS"

                Warning ->
                    "WARNING"

                Errored ->
                    "ERROR"

        affected =
            if List.isEmpty c.affectedResources then
                ""

            else
                "\n   Affected:\n"
                    ++ (c.affectedResources
                            |> List.take 8
                            |> List.map (\r -> "     - " ++ r)
                            |> String.join "\n"
                       )

        fix =
            case c.howToFix of
                Just s ->
                    "\n   Hint: " ++ s

                Nothing ->
                    ""
    in
    n ++ ". [" ++ sev ++ "] " ++ c.name ++ " — " ++ c.summary ++ affected ++ fix
