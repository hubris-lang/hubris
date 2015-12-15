module HEdit where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Json.Decode as Json
import Signal exposing (Signal, Address)
import String
import Window
import Graphics.Input.Field exposing (..)

name : Signal.Mailbox Content
name = Signal.mailbox noContent

type alias Model = {
    position : Int
}

initialModel : Model
initialModel = { position = 0 }

type Action = Enter

update : Action -> Model -> Model
update action model = model

view : Address Action -> Model -> Html
view address model = div [] [text "Hello World"]

main : Signal Html
main = Signal.map (view actions.address) model

model : Signal Model
model = Signal.foldp update initialModel actions.signal

actions : Signal.Mailbox Action
actions = Signal.mailbox Enter
