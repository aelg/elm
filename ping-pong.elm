
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
--import Graphics.Element exposing (flow, right, middle)
import Signal exposing (map, foldp)
import Color exposing (..)
import Mouse
import Time
import Html


----- Model -----
type alias Pos =
  { x : Float
  , y : Float
  }


type alias Ball = 
  { p : (Float, Float)
  , v : (Float, Float)
  , size : Float
  , color : Color
  , lastP : (Float, Float)
  }


type alias Paddle = 
  { width : Float
  , height : Float
  , pos : Float
  , color : Color
  }

type alias Canvas = 
  { width : Float
  , height : Float
  , paddlePadding : Float
  , color : Color
  }

type State = 
  Running | Paused

type alias Model =
  { canvas  : Canvas
  , leftPaddle : Paddle
  , rightPaddle : Paddle
  , ball : Ball
  , score : (Int, Int)
  , aiSpeed : Float
  , speedIncrease : Float
  , state : State
  }

model0 : Model
model0 = 
  { canvas = Canvas 800 500 40 black
  , rightPaddle = Paddle 10 100 0 white
  , leftPaddle = Paddle 10 100 0 white
  , ball = Ball (0,0) (-400,100) 10 white (0, 0)
  , score = (0, 0)
  , aiSpeed = 200
  , speedIncrease = 10
  , state = Paused
  }

----- Actions -----
type Action = 
  NoOp
  | MouseMove (Float, Float)
  | MouseClick
  | Tick Time.Time

getBorders : Canvas -> (Float, Float)
getBorders canvas = (canvas.width/2, canvas.height/2)

getMaxX = getBorders >> fst
getMaxY = getBorders >> snd

absoluteToCanvasRelative : Canvas -> (Float, Float) -> (Float, Float)
absoluteToCanvasRelative canvas (x, y) =
  let (maxX, maxY) = getBorders canvas
  in (x - maxX, -(y - maxY))

--- Paddle movement ---
paddleMaxPos : Canvas -> Paddle -> Float
paddleMaxPos canvas paddle = 
  getMaxY canvas - paddle.height/2

updatePaddlePos : Canvas -> Float -> Paddle -> Paddle
updatePaddlePos canvas y paddle =
  let maxPos = paddleMaxPos canvas paddle
  in {paddle | pos = min maxPos <| max -maxPos y}

onMouseMove : Model -> (Float, Float) -> Paddle
onMouseMove model (_, y) =
  if model.state == Running then
    updatePaddlePos model.canvas y model.leftPaddle
  else
    model.leftPaddle

--- AI movement ---
moveAI : Time.Time -> Model -> Model
moveAI timeDiff model = 
  let (_,y) = model.ball.p
      rightPaddle = model.rightPaddle
      aiPos = rightPaddle.pos
  in 
     if aiPos > y + model.aiSpeed * Time.inSeconds timeDiff then 
        { model | rightPaddle = 
            updatePaddlePos 
              model.canvas 
              (aiPos - model.aiSpeed * Time.inSeconds timeDiff)
              rightPaddle
        }
     else if aiPos < y - model.aiSpeed * Time.inSeconds timeDiff then 
        { model | rightPaddle = 
            updatePaddlePos 
              model.canvas 
              (aiPos + model.aiSpeed * Time.inSeconds timeDiff)
              rightPaddle
        }
     else model

--- Ball movement ---
moveBall : Time.Time -> Model -> Model
moveBall timeDiff model = 
  let ball = model.ball
      (x,y) = ball.p
      (vx,vy) = ball.v
  in {model | ball = {ball | p = 
       ( x + vx * Time.inSeconds timeDiff
       , y + vy * Time.inSeconds timeDiff
       )
     , lastP = ball.p
     }}

bounceX : Ball -> Ball
bounceX ball = 
  {ball | v = (\(a,b) -> (negate a, b)) ball.v}

bounceY : Ball -> Ball
bounceY ball = 
  {ball | v = (\(a,b) -> (a, negate b)) ball.v}

-- TODO : Make nicer
shouldBounceLeftPaddle : Model -> Ball -> Bool
shouldBounceLeftPaddle model ball = 
  let (x,y) = ball.p
      (lx,_) = ball.lastP
      (vx,_) = ball.v
      paddleEdge = 
        -((getMaxX model.canvas) - model.canvas.paddlePadding) + model.leftPaddle.width/2
      paddleTop = model.leftPaddle.pos + model.leftPaddle.height/2
      paddleBottom = model.leftPaddle.pos - model.leftPaddle.height/2
  in vx < 0 
  && x < paddleEdge + ball.size
  && lx >= paddleEdge + ball.size
  && y <= paddleTop
  && y >= paddleBottom

shouldBounceRightPaddle : Model -> Ball -> Bool
shouldBounceRightPaddle model ball = 
  let (x,y) = ball.p
      (lx,_) = ball.lastP
      (vx,_) = ball.v
      paddleEdge = 
        (getMaxX model.canvas) - model.canvas.paddlePadding - model.rightPaddle.width/2
      paddleTop = model.rightPaddle.pos + model.rightPaddle.height/2
      paddleBottom = model.rightPaddle.pos - model.rightPaddle.height/2
  in vx > 0 
  && x > paddleEdge - ball.size
  && lx <= paddleEdge - ball.size
  && y <= paddleTop
  && y >= paddleBottom

shouldBounceY : Model -> Ball -> Bool
shouldBounceY model ball =
  let (_,y) = ball.p
      (_,vy) = ball.v
  in
     vy < 0 && y < -(getMaxY model.canvas - ball.size)
  || vy > 0 && y > getMaxY model.canvas - ball.size

changeY : Model -> Paddle -> Ball -> Ball
changeY model paddle ball = 
  let (_,y) = ball.p
      (vx,vy) = ball.v
      offset = paddle.pos - y
  in { ball | v = (vx, vy-offset) }

ballMovement : Model -> Model
ballMovement model = 
  let ball = model.ball 
      (x,y) = ball.p
      (vx, vy) = ball.v
  in {model | ball = 
     if shouldBounceLeftPaddle model ball then 
        ball 
          |> bounceX
          |> changeY model model.leftPaddle
     else if shouldBounceRightPaddle model ball then 
        ball 
          |> bounceX
          |> changeY model model.rightPaddle
     else if shouldBounceY model ball then 
        ball 
          |> bounceY
     else ball
     }

gameLost : Model -> Model
gameLost model = 
  let (x,_) = model.ball.p
  in if x < -(getMaxX model.canvas) then 
        { model | 
          score = (fst model.score, snd model.score + 1)
        , ball = model0.ball
        }
     else if x > getMaxX model.canvas then 
        { model | 
          score = (fst model.score + 1, snd model.score)
        , ball = model0.ball
        }
     else model

increaseSpeed : Time.Time -> Model -> Model
increaseSpeed timeDiff model = 
  let ball = model.ball
      (vx,vy) = ball.v
  in { model | ball = 
       { ball | v = 
         (vx + model.speedIncrease * (vx/abs vx) * Time.inSeconds timeDiff, vy)}}

onTick : Model -> Time.Time -> Model
onTick model timeDiff = 
  if model.state == Running
     then
  model
    |> moveAI timeDiff
    |> moveBall timeDiff
    |> ballMovement
    |> increaseSpeed timeDiff
    |> gameLost
    else model

toggleState : Model -> Model
toggleState model = 
  if model.state == Running
  then { model | state = Paused }
  else { model | state = Running }

--- Update ---
update : Action -> Model -> Model
update action model = 
  case action of
    NoOp -> model
    MouseMove pos -> 
      {model | leftPaddle = onMouseMove model <| absoluteToCanvasRelative model.canvas pos}
    MouseClick -> toggleState model
    Tick time -> onTick model time

----- View -----
paddle : Paddle -> Form
paddle paddle = 
  rect paddle.width paddle.height
    |> filled paddle.color
    |> moveY paddle.pos

leftPaddle : Model -> Form
leftPaddle model =
  paddle model.leftPaddle
    |> moveX -((getMaxX model.canvas) - model.canvas.paddlePadding)

rightPaddle : Model -> Form
rightPaddle model =
  paddle model.rightPaddle
    |> moveX (getMaxX model.canvas - model.canvas.paddlePadding)

ball : Ball -> Form
ball ball =
  circle ball.size
    |> filled ball.color
    |> move ball.p

canvas : Canvas -> List Form -> Element
canvas canvas forms = 
  collage (floor canvas.width) (floor canvas.height) forms 
  |> color canvas.color

score : (Int, Int) -> Element
score score = 
  flow down 
    [ show ("Left Player: " ++ (toString <| fst score))
    , show ("Right Player: " ++  (toString <| snd score))
    ]

view : Model -> Element
view model = 
  flow down
    [ canvas model.canvas
        [ leftPaddle model
        , rightPaddle model
        , ball model.ball
        ]
    , score model.score
    ]

----- Wiring -----
mailbox : Signal.Mailbox Action
mailbox = Signal.mailbox NoOp

mouse : (Int, Int) -> Action
mouse (x, y) = MouseMove (toFloat x, toFloat y)

mouseClick : () -> Action
mouseClick () = MouseClick

ticker : Time.Time -> Action
ticker time = Tick time

mergeSignals : Signal Action
mergeSignals =
  Signal.mergeMany 
  [ Signal.map identity mailbox.signal
  , Signal.map mouse Mouse.position
  , Signal.map mouseClick Mouse.clicks
  , Signal.map ticker (Time.fps 40)
  ]


model : Signal Model
model = Signal.foldp update model0 mergeSignals

main : Signal Element
main = Signal.map view model

