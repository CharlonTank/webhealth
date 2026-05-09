module Evergreen.V26.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Lamdera
import Time
import Url


type Page
    = Home
    | HistoryPage
    | SitePage String


type Severity
    = Pass
    | Warning
    | Errored


type alias Check =
    { id : String
    , name : String
    , severity : Severity
    , summary : String
    , affectedResources : List String
    , howToFix : Maybe String
    , extra : List ( String, String )
    }


type alias Category =
    { name : String
    , checks : List Check
    }


type alias AuditReport =
    { url : String
    , finalUrl : String
    , scannedAt : Time.Posix
    , perceivedLoadMs : Int
    , totalTestMs : Int
    , score : Int
    , passed : Int
    , warnings : Int
    , errors : Int
    , categories : List Category
    }


type AuditStatus
    = Idle
    | Running String
    | Done AuditReport
    | Failed String


type alias HistoryEntry =
    { url : String
    , finalUrl : String
    , scannedAt : Time.Posix
    , score : Int
    , passed : Int
    , warnings : Int
    , errors : Int
    , report : AuditReport
    }


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , page : Page
    , urlInput : String
    , status : AuditStatus
    , history : List HistoryEntry
    , historyQuery : String
    , now : Time.Posix
    , excludedIssues : List String
    , promptCopied : Bool
    , hoveredDot : Maybe Int
    }


type alias ProbeResult =
    { status : Int
    , bodyLen : Int
    , url : String
    }


type alias InflightAudit =
    { url : String
    , clientId : Lamdera.ClientId
    , started : Time.Posix
    , htmlBody : Maybe String
    , htmlHeaders : Maybe (List ( String, String ))
    , htmlStatus : Maybe Int
    , htmlMillis : Maybe Int
    , finalUrl : Maybe String
    , robots : Maybe ProbeResult
    , sitemap : Maybe ProbeResult
    , favicon : Maybe ProbeResult
    , externalLinks : Dict.Dict String (Maybe Int)
    , internalLinks : Dict.Dict String (Maybe Int)
    }


type alias BackendModel =
    { history : List HistoryEntry
    , inflight : Dict.Dict String InflightAudit
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | UrlInputChanged String
    | AnalyzeClicked
    | HistoryQueryChanged String
    | OpenHistoryEntry HistoryEntry
    | ToggleIssue String
    | CopyFixPrompt String
    | PromptCopiedMsg
    | HoverDot Int
    | UnhoverDot
    | Tick Time.Posix


type ToBackend
    = NoOpToBackend
    | RequestAudit String
    | RequestHistory


type BackendMsg
    = NoOpBackendMsg
    | AuditFinished Lamdera.ClientId (Result String AuditReport)


type ToFrontend
    = NoOpToFrontend
    | AuditStarted String
    | AuditCompleted AuditReport
    | AuditFailed String
    | HistoryUpdated (List HistoryEntry)
