
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
--import Graphics.Element exposing (flow, right, middle)
import Signal exposing (map, foldp)
import Color exposing (..)
import Mouse
import Time

type Action = 
  NoOp
  | MouseMove (Float, Float)
  | Tick Time.Time


type alias Pos =
  {
    x : Float
  , y : Float
  }


type alias Model =
  {
    canvasWidth : Float
  , canvasHeight : Float
  , backgroundColor : Color
  , foregroundColor : Color
  , leftPos : Float
  , rightPos : Float
  , rightPos : Float
  , ballPos : Pos
  , ballVelocity : Pos
  }


model0 : Model
model0 = 
  {
    canvasWidth = 800
  , canvasHeight = 500
  , backgroundColor = black
  , foregroundColor = white
  , rightPos = 0
  , leftPos = 100
  , ballPos = {x = 0, y = 0}
  , ballVelocity = {x = 100, y = 40}
  }


absoluteToCanvasRelative : Model -> (Float, Float) -> (Float, Float)
absoluteToCanvasRelative model (x, y) =
  (x - model.canvasWidth/2, -(y - model.canvasHeight/2))


onMouseMove : Model -> (Float, Float) -> Model
onMouseMove model (x, y) =
  {model | leftPos = y}


onTick : Model -> Time.Time -> Model
onTick model timeDiff = 
  let 
    pos = model.ballPos
    velocity = model.ballVelocity
  in
    {
      model | 
        ballPos = 
          {
            x = pos.x + velocity.x * Time.inSeconds timeDiff
          , y = pos.y + velocity.y * Time.inSeconds timeDiff
          }
    }
    |> physics

physics : Model -> Model
physics model = 
  let 
    pos = model.ballPos
    velocity = model.ballVelocity
  in
    if abs pos.x > model.canvasWidth/2
    then
      {
        model | 
          ballVelocity = 
            { 
              x = -velocity.x
            , y = velocity.y
            }
      }
    else if abs pos.y > model.canvasHeight/2
      then
        {
          model | 
            ballVelocity = 
              { 
                x = velocity.x
              , y = -velocity.y
              }
        }
        else model

update : Action -> Model -> Model
update action model = 
  case action of
    NoOp -> model
    MouseMove pos -> onMouseMove model <| absoluteToCanvasRelative model pos
    Tick time -> onTick model time


paddle : Model -> Form
paddle model = 
  rect 10 100
    |> filled model.foregroundColor


leftPaddle : Model -> Form
leftPaddle model =
  paddle model
    |> moveX -(model.canvasWidth/2 - 20)
    |> moveY model.leftPos


rightPaddle : Model -> Form
rightPaddle model =
  paddle model
    |> moveX (model.canvasWidth/2 - 20)
    |> moveY model.rightPos


ball : Model -> Form
ball model =
  circle 10
    |> filled model.foregroundColor
    |> moveX model.ballPos.x
    |> moveY model.ballPos.y


view : Model -> Element
view model = 
  flow right 
    [ 
      collage (floor model.canvasWidth) (floor model.canvasHeight)
        [
          leftPaddle model
        , rightPaddle model
        , ball model
        ]
        |> color model.backgroundColor
    ]


mailbox : Signal.Mailbox Action
mailbox = Signal.mailbox NoOp


mouse : (Int, Int) -> Action
mouse (x, y) = MouseMove (toFloat x, toFloat y)


ticker : Time.Time -> Action
ticker time = Tick time


mergeSignals : Signal Action
mergeSignals =
  Signal.mergeMany 
  [
    Signal.map identity mailbox.signal,
    Signal.map mouse Mouse.position,
    Signal.map ticker (Time.fps 40)
  ]


model : Signal Model
model = Signal.foldp update model0 mergeSignals


main : Signal Element
main = Signal.map view model

