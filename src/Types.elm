module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Http
import Lamdera exposing (ClientId, SessionId)
import Time
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
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


type alias BackendModel =
    { history : List HistoryEntry
    , inflight : Dict String InflightAudit
    }


type alias InflightAudit =
    { url : String
    , clientId : ClientId
    , started : Time.Posix
    , htmlBody : Maybe String
    , htmlHeaders : Maybe (List ( String, String ))
    , htmlStatus : Maybe Int
    , htmlMillis : Maybe Int
    , finalUrl : Maybe String
    , robots : Maybe ProbeResult
    , sitemap : Maybe ProbeResult
    , favicon : Maybe ProbeResult
    , externalLinks : Dict String (Maybe Int)
    , internalLinks : Dict String (Maybe Int)
    }


type alias ProbeResult =
    { status : Int
    , bodyLen : Int
    , url : String
    }


type Page
    = Home
    | HistoryPage
    | SitePage String


type AuditStatus
    = Idle
    | Running String
    | Done AuditReport
    | Failed String


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


type alias Category =
    { name : String
    , checks : List Check
    }


type alias Check =
    { id : String
    , name : String
    , severity : Severity
    , summary : String
    , affectedResources : List String
    , howToFix : Maybe String
    , extra : List ( String, String )
    }


type Severity
    = Pass
    | Warning
    | Errored


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


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
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
    | AuditFinished ClientId (Result String AuditReport)


type alias HtmlFetch =
    { status : Int
    , body : String
    , headers : List ( String, String )
    , finalUrl : String
    , millis : Int
    }


type ToFrontend
    = NoOpToFrontend
    | AuditStarted String
    | AuditCompleted AuditReport
    | AuditFailed String
    | HistoryUpdated (List HistoryEntry)
