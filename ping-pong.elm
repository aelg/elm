
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
--import Graphics.Element exposing (flow, right, middle)
import Signal
import Color exposing (..)
import Mouse
import Keyboard
import Time
import Text


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
  { name : String
  , width : Float
  , height : Float
  , x : Float
  , y : Float
  , score : Int
  , color : Color
  , controlledBy : Controls
  , speed : Float
  }

type alias Canvas = 
  { width : Float
  , height : Float
  , color : Color
  }

type State = 
  Running | Paused

type Controls = 
  MouseControls 
  | ArrowControls 
  | WasdControls 
  | Computer

type alias Model =
  { canvas  : Canvas
  , leftPaddle : Paddle
  , rightPaddle : Paddle
  , ball : Ball
  , score : (Int, Int)
  , speedIncrease : Float
  , state : State
  , mouseY : Float
  , arrowDirection : Int
  , wasdDirection : Int
  }

model0 : Model
model0 = 
  { canvas = Canvas 800 500 black
  , leftPaddle = Paddle "Left Player" 10 100 -360 0 0 white WasdControls 200
  , rightPaddle = Paddle "Right Player" 10 100 360 0 0 white ArrowControls 200
  , ball = Ball (320,0) (-400,100) 10 white (320, 0)
  , score = (0, 0)
  , speedIncrease = 10
  , state = Paused
  , mouseY = 0
  , arrowDirection = 0
  , wasdDirection = 0
  }

----- Actions -----
type Action = 
  NoOp
  | MouseMove (Int, Int)
  | MouseClick
  | Tick Time.Time
  | ArrowPressed {x:Int, y:Int}
  | WasdPressed {x:Int, y:Int}
  | SpacePressed Bool

getBorders : Canvas -> (Float, Float)
getBorders canvas = (canvas.width/2, canvas.height/2)

getMaxX : Canvas -> Float
getMaxX = getBorders >> fst
getMaxY : Canvas -> Float
getMaxY = getBorders >> snd

absoluteToCanvasRelative : Canvas -> (Int, Int) -> (Float, Float)
absoluteToCanvasRelative canvas (x, y) =
  let (maxX, maxY) = getBorders canvas
  in (toFloat x - maxX, -(toFloat y - maxY))

--- Paddle movement ---
paddleMaxPos : Canvas -> Paddle -> Float
paddleMaxPos canvas paddle = 
  getMaxY canvas - paddle.height/2

updatePaddlePos : Canvas -> Float -> Paddle -> Paddle
updatePaddlePos canvas newY paddle =
  let maxPos = paddleMaxPos canvas paddle
  in {paddle | y = clamp (-maxPos) maxPos newY}

onMouseMove : Model -> (Int, Int) -> Model
onMouseMove model pos =
  let (_,y) = absoluteToCanvasRelative model.canvas pos
  in { model | mouseY = y }

onArrowPressed : Model -> {x : Int, y : Int} -> Model
onArrowPressed model {y} = 
  { model | arrowDirection = y }

onWasdPressed : Model -> {x : Int, y : Int} -> Model
onWasdPressed model {y} = 
  { model | wasdDirection = y }

--- AI movement ---
moveAI : Time.Time -> Model -> Paddle -> Paddle
moveAI timeDiff model paddle = 
  let (_,y) = model.ball.p
      aiPos = paddle.y
      diff = abs (aiPos - y)
      direction = if aiPos > y then -1 else 1
  in 
     if diff >= 1 then
            updatePaddlePos 
              model.canvas 
              (aiPos + min diff paddle.speed * direction * Time.inSeconds timeDiff)
              paddle
     else
        paddle

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

shouldBouncePaddle : Model -> Paddle -> Ball -> Bool
shouldBouncePaddle model paddle ball = 
  let (x,y) = ball.p
      (lx,_) = ball.lastP
      (vx,_) = ball.v
      paddleEdge = if vx > 0 
        then paddle.x - paddle.width/2
        else paddle.x + paddle.width/2
      ballEdgeOffset = if vx > 0
        then ball.size
        else -ball.size
      paddleTop = paddle.y + paddle.height/2
      paddleBottom = paddle.y - paddle.height/2
  in ((x + ballEdgeOffset > paddleEdge && lx +ballEdgeOffset <= paddleEdge)
   || (x + ballEdgeOffset < paddleEdge && lx +ballEdgeOffset >= paddleEdge))
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
      offset = paddle.y - y
  in { ball | v = (vx, vy-offset) }

bouncePaddle : Model -> Paddle -> Ball -> Ball
bouncePaddle model paddle ball = 
  if shouldBouncePaddle model paddle ball then
     ball
       |> bounceX
       |> changeY model paddle
  else ball

bounceWalls : Model -> Ball -> Ball
bounceWalls model ball = 
  if shouldBounceY model ball then bounceY ball
  else ball

ballCollision : Model -> Model
ballCollision model = 
  {model | ball = 
    model.ball 
      |> bouncePaddle model model.leftPaddle
      |> bouncePaddle model model.rightPaddle
      |> bounceWalls model
  }

gameLost : Model -> Model
gameLost model = 
  let (x,_) = model.ball.p
      leftPaddle = model.leftPaddle
      rightPaddle = model.rightPaddle
  in if x < -(getMaxX model.canvas) then 
        { model | rightPaddle = { rightPaddle | score = rightPaddle.score + 1 }
          , ball = model0.ball
        }
     else if x > getMaxX model.canvas then 
        { model | leftPaddle = { leftPaddle | score = leftPaddle.score + 1 }
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

movePaddleKeyboard : Model -> Time.Time -> Int -> Paddle -> Paddle
movePaddleKeyboard model timeDiff direction paddle = 
  let newPos = paddle.y + paddle.speed * Time.inSeconds timeDiff * toFloat direction
  in updatePaddlePos model.canvas newPos paddle

movePaddle : Time.Time -> Model -> Paddle -> Paddle
movePaddle timeDiff model paddle = 
  case paddle.controlledBy of
    Computer -> moveAI timeDiff model paddle
    MouseControls -> updatePaddlePos model.canvas model.mouseY paddle
    ArrowControls -> movePaddleKeyboard model timeDiff model.arrowDirection paddle
    WasdControls -> movePaddleKeyboard model timeDiff model.wasdDirection paddle

movePaddles : Time.Time -> Model -> Model
movePaddles timeDiff model = 
  { model | leftPaddle = movePaddle timeDiff model model.leftPaddle
  , rightPaddle = movePaddle timeDiff model model.rightPaddle
  }

onTick : Model -> Time.Time -> Model
onTick model timeDiff = 
  if model.state == Running
     then
  model
    |> moveBall timeDiff
    |> movePaddles timeDiff
    |> ballCollision
    |> increaseSpeed timeDiff
    |> gameLost
  else model

toggleState : Bool -> Model -> Model
toggleState do model = 
  if do then 
    if model.state == Running
    then { model | state = Paused }
    else { model | state = Running }
  else model

--- Update ---
update : Action -> Model -> Model
update action model = 
  case action of
    NoOp -> model
    MouseMove pos -> onMouseMove model pos
    MouseClick -> toggleState True model
    ArrowPressed dir -> onArrowPressed model dir
    WasdPressed dir -> onWasdPressed model dir
    SpacePressed pressed -> toggleState pressed model
    Tick time -> onTick model time

----- View -----
paddle : Paddle -> Form
paddle paddle = 
  rect paddle.width paddle.height
    |> filled paddle.color
    |> moveY paddle.y
    |> moveX paddle.x

ball : Ball -> Form
ball ball =
  circle ball.size
    |> filled ball.color
    |> move ball.p

shadowBall : Ball -> Form
shadowBall ball =
  circle ball.size
    |> filled ball.color
    |> alpha 0.5
    |> move ball.lastP

canvas : Canvas -> List Form -> Element
canvas canvas forms = 
  collage (floor canvas.width) (floor canvas.height) forms 
  |> color canvas.color

scoreText : Color -> String -> Element
scoreText color s =
  s
    |> Text.fromString
    |> Text.height 30
    |> Text.monospace
    |> Text.color color
    |> leftAligned

scoreColor : Int -> Int -> Color
scoreColor myScore theirScore = 
  if myScore > theirScore then green 
  else if myScore < theirScore then red 
  else yellow

paddleScore : Paddle -> Color -> Element
paddleScore paddle color = 
  scoreText color ( paddle.name ++ ": " ++ toString paddle.score)

score : Model -> Element
score model = 
  let canvasWidth = floor model.canvas.width
      leftColor = scoreColor model.leftPaddle.score model.rightPaddle.score
      rightColor = scoreColor model.rightPaddle.score model.leftPaddle.score
  in flow down 
    [ spacer canvasWidth 1
    , flow right 
      [ paddleScore model.leftPaddle leftColor
      , spacer 100 1
      , paddleScore model.rightPaddle rightColor
      ]
        |> container canvasWidth 100 middle
        |> color black
    ]

view : Model -> Element
view model = 
  flow down
    [ canvas model.canvas
        [ paddle model.leftPaddle
        , paddle model.rightPaddle
        , shadowBall model.ball
        , ball model.ball
        ]
    , score model
    ]

----- Wiring -----
mergeSignals : Signal Action
mergeSignals =
  Signal.mergeMany 
  [ Signal.map MouseMove Mouse.position
  , Signal.map ArrowPressed Keyboard.arrows
  , Signal.map WasdPressed Keyboard.wasd
  , Signal.map SpacePressed Keyboard.space
  , Signal.map (always MouseClick) Mouse.clicks
  , Signal.map Tick (Time.fps 100)
  ]


model : Signal Model
model = Signal.foldp update model0 mergeSignals

main : Signal Element
main = Signal.map view model

