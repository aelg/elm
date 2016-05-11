
import Graphics.Element exposing (flow, right, down, show)
import Signal

type Action = NoOp


type alias Model =
  {}

update : Action -> Model -> Model
update action model = 
  case action of
    NoOp -> model


view : Model -> Graphics.Element.Element
view model = 
  flow right [show "HelloWorld"]


model0 : Model
model0 = {}


mailbox : Signal.Mailbox Action
mailbox = Signal.mailbox NoOp


model : Signal Model
model = Signal.foldp update model0 mailbox.signal


main : Signal Graphics.Element.Element
main = Signal.map view model

