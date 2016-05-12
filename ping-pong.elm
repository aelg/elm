
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
  , color : Color
  }

type alias Model =
  { canvas  : Canvas
  , leftPaddle : Paddle
  , rightPaddle : Paddle
  , ball : Ball
  , score : (Int, Int)
  , aiSpeed : Float
  }

model0 : Model
model0 = 
  { canvas = Canvas 800 500 black
  , rightPaddle = Paddle 10 100 0 white
  , leftPaddle = Paddle 10 100 0 white
  , ball = Ball (0,0) (-100,40) 10 white (0, 0)
  , score = (0, 0)
  , aiSpeed = 30
  }

----- Actions -----
type Action = 
  NoOp
  | MouseMove (Float, Float)
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
  updatePaddlePos model.canvas y model.leftPaddle

--- AI movement ---
moveAI : Time.Time -> Model -> Model
moveAI timeDiff model = 
  let (_,y) = model.ball.p
      rightPaddle = model.rightPaddle
      aiPos = rightPaddle.pos
  in if aiPos > y
       then { model | rightPaddle = {rightPaddle | pos = 
              aiPos - model.aiSpeed * Time.inSeconds timeDiff
            }}
     else if aiPos < y
       then { model | rightPaddle = {rightPaddle | pos = 
              aiPos + model.aiSpeed * Time.inSeconds timeDiff
            }}
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

shouldBounceLeftPaddle : Model -> Ball -> Bool
shouldBounceLeftPaddle model ball = 
  let (x,y) = ball.p
      (lx,_) = ball.lastP
      (vx,_) = ball.v
      paddleEdge = -((getMaxX model.canvas) - 20) + model.leftPaddle.width/2
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
      paddleEdge = (getMaxX model.canvas) - 20 - model.rightPaddle.width/2
      paddleTop = model.rightPaddle.pos + model.rightPaddle.height/2
      paddleBottom = model.rightPaddle.pos - model.rightPaddle.height/2
  in vx > 0 
  && x > paddleEdge - ball.size
  && lx <= paddleEdge - ball.size
  && y <= paddleTop
  && y >= paddleBottom

shouldBounceY : Model -> Ball -> Bool
shouldBounceY model ball=
  let (_,y) = ball.p
      (_,vy) = ball.v
  in
     vy < 0 && y < -(getMaxY model.canvas - ball.size)
  || vy > 0 && y > getMaxY model.canvas - ball.size

physics : Model -> Model
physics model = 
  let ball = model.ball 
      (x,y) = ball.p
      (vx, vy) = ball.v
  in {model | ball = 
     if shouldBounceLeftPaddle model ball || shouldBounceRightPaddle model ball
       then bounceX ball
     else if shouldBounceY model ball
       then bounceY ball
     else ball
   }

gameLost : Model -> Model
gameLost model = 
  let (x,_) = model.ball.p
  in if x < -(getMaxX model.canvas)
       then { model | 
                score = (fst model.score, snd model.score + 1)
              , ball = model0.ball
              }
     else if x > getMaxX model.canvas
       then { model | 
                score = (fst model.score + 1, snd model.score)
              , ball = model0.ball
            }
     else model

onTick : Model -> Time.Time -> Model
onTick model timeDiff = 
  model
    |> moveAI timeDiff
    |> moveBall timeDiff
    |> physics
    |> gameLost

--- Update ---
update : Action -> Model -> Model
update action model = 
  case action of
    NoOp -> model
    MouseMove pos -> 
      {model | leftPaddle = onMouseMove model <| absoluteToCanvasRelative model.canvas pos}
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
    |> moveX -((getMaxX model.canvas) - 20)

rightPaddle : Model -> Form
rightPaddle model =
  paddle model.rightPaddle
    |> moveX (getMaxX model.canvas - 20)

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

ticker : Time.Time -> Action
ticker time = Tick time

mergeSignals : Signal Action
mergeSignals =
  Signal.mergeMany 
  [ Signal.map identity mailbox.signal
  , Signal.map mouse Mouse.position
  , Signal.map ticker (Time.fps 40)
  ]


model : Signal Model
model = Signal.foldp update model0 mergeSignals

main : Signal Element
main = Signal.map view model

