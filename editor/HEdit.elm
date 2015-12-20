module HEdit where

import Http exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Json.Decode as Json
import Signal exposing (Signal, Address)
import String
import Task exposing (..)
import Window

type alias Model = {
    position : Int,
    buffer : String
}

initialModel : Model
initialModel = {
  position = 0,
  buffer = ""
}

type Action = Enter
            | NewText String
            | Error

update : Action -> Model -> Model
update action model =
  case action of
    Enter -> { model | position = model.position + 1 }
    NewText s -> { model | buffer = s }

onInput : Signal.Address Action -> Attribute
onInput address =
  on "input" targetValue (\s -> Signal.message address (NewText s))

compilerQueries : Signal.Mailbox String
compilerQueries = Signal.mailbox ""

compilerResults : Signal.Mailbox (Result Error String)
compilerResults =
  Signal.mailbox (Ok "")

typecheckCode : String -> Task Error String
typecheckCode code = getString "http://localhost:8001/"

port compilerRequests : Signal (Task x ())
port compilerRequests =
  Signal.map typecheckCode compilerQueries.signal
    |> Signal.map (\task -> Task.toResult task `andThen` Signal.send compilerResults.address)

view : Address String -> Model -> Html
view address model = div [class "border"]
  [textarea [onInput address, class "text-buffer"] [],
   textarea [class "text-buffer"] [],
   button [onClick address ""] [text "Check"],
   div [] [text (toString model.position)],
   div [] [text (toString model.buffer)]]

main : Signal Html
main = Signal.map2 view compilerQueries.address compilerResults.signal model

model : Signal Model
model = Signal.foldp update initialModel actions.signal

actions : Signal.Mailbox Action
actions = Signal.mailbox Enter
