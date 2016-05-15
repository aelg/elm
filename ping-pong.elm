import Html
import Html.App
import Collage exposing (..)
import Element exposing (..)
import Color exposing (..)
import Time
import Text
import Mouse
import Keyboard
import AnimationFrame
import Platform.Sub
import Char

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
  , controlledBy : PlayerType
  , speed : Float
  }

type alias Canvas =
  { width : Int
  , height : Int
  , color : Color
  }

type State =
  Running | Paused

type PlayerType =
  MouseControls
  | ArrowControls
  | WasdControls
  | EasyComputer
  | HardComputer
  | InsaneComputer

type alias Model =
  { canvas  : Canvas
  , scoreHeight : Int
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
  , scoreHeight = 100
  , leftPaddle = Paddle "Left Player" 10 100 -360 0 0 white WasdControls 200
  , rightPaddle = Paddle "Right Player" 10 100 360 0 0 white EasyComputer 200
  , ball = Ball (320,0) (-400,100) 10 white (320, 0)
  , score = (0, 0)
  , speedIncrease = 10
  , state = Paused
  , mouseY = 0
  , arrowDirection = 0
  , wasdDirection = 0
  }

----- Msgs -----
type Msg =
  NoOp
  | MouseMove (Int, Int)
  | Tick Time.Time
  | ArrowPressed Int
  | WasdPressed Int
  | SpacePressed Bool
  | ChangeLeftPlayer
  | ChangeRightPlayer

clampFold mi ma n = if n > ma then
                       clampFold mi ma (ma - (n - ma)) else
                    if n < mi then
                       clampFold mi ma (mi + (mi - n)) else
                       n

getBorders : Canvas -> (Float, Float)
getBorders canvas = (toFloat canvas.width / 2, toFloat canvas.height / 2)

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

movePaddle : Time.Time -> Model -> Float -> Paddle -> Paddle
movePaddle timeDiff model newY paddle =
  let diff = newY - paddle.y
      maxDiff = paddle.speed * Time.inSeconds timeDiff
      restrictedDiff = clamp (-maxDiff) maxDiff diff
      newPos = paddle.y + restrictedDiff
  in updatePaddlePos model.canvas newPos paddle

onMouseMove : Model -> (Int, Int) -> Model
onMouseMove model pos =
  let (_,y) = absoluteToCanvasRelative model.canvas pos
  in { model | mouseY = y }

onArrowPressed : Model -> Int -> Model
onArrowPressed model y =
  { model | arrowDirection = y }

onWasdPressed : Model -> Int -> Model
onWasdPressed model y =
  { model | wasdDirection = y }

--- AI movement ---
moveInsaneAI : Time.Time -> Model -> Float -> Paddle -> Paddle
moveInsaneAI timeDiff model aggressiveness paddle =
  let (ballx,bally) = model.ball.p
      (ballvx, ballvy) = model.ball.v
      magnitude x y = sqrt (x * x + y * y)
      toward = ballx < paddle.x && ballvx > 0 || ballx > paddle.x && ballvx < 0
      speedUpOffset = if ballvy < 0 then
                         (paddle.height/2) * aggressiveness else
                         -(paddle.height/2) * aggressiveness
      dY = ballvy / abs ballvx
      sizeOffset = model.ball.size + paddle.width/2
      deltaX = if toward then
                  abs (ballx - paddle.x) - sizeOffset else
                  2*(abs (model.leftPaddle.x - model.rightPaddle.x) - sizeOffset) - abs (ballx - paddle.x)
      maxY = getMaxY model.canvas - model.ball.size
      hitPos = clampFold -maxY maxY (dY * deltaX + bally + speedUpOffset)
  in movePaddle timeDiff model hitPos paddle

moveAI : Time.Time -> Model -> Float -> Paddle -> Paddle
moveAI timeDiff model aggressiveness paddle =
  let (ballx,bally) = model.ball.p
      (ballvx, ballvy) = model.ball.v
      toward = ballx < paddle.x && ballvx > 0 || ballx > paddle.x && ballvx < 0
      speedUpOffset = if ballvy < 0 then
                      (paddle.height/2) * aggressiveness else
                      -(paddle.height/2) * aggressiveness
      restPos = if ballvy > 0 then
                   getMaxY model.canvas/2 else
                   -(getMaxY model.canvas/2)
      targetPos = bally + speedUpOffset
  in if toward then movePaddle timeDiff model targetPos paddle
     else movePaddle timeDiff model restPos paddle

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

bounceY : Canvas -> Ball -> Ball
bounceY canvas ball =
  let maxY = getMaxY canvas - ball.size
  in { ball |
       v = (\(a,b) -> (a, negate b)) ball.v
     , p = (\(a,b) -> (a, clampFold -maxY maxY b)) ball.p
  }

shouldBouncePaddle : Model -> Paddle -> Ball -> Bool
shouldBouncePaddle model paddle ball =
  let (x,y) = ball.p
      (lx,ly) = ball.lastP
      (vx,_) = ball.v
      paddleEdge = if vx > 0
        then paddle.x - paddle.width/2
        else paddle.x + paddle.width/2
      ballEdgeOffset = if vx > 0
        then ball.size
        else -ball.size
      paddleTop = paddle.y + paddle.height/2
      paddleBottom = paddle.y - paddle.height/2
      isBetween a (x, y) = a >= x && a <= y || a >= y && a <= x
  in ((x + ballEdgeOffset > paddleEdge && lx + ballEdgeOffset <= paddleEdge)
      || (x + ballEdgeOffset < paddleEdge && lx + ballEdgeOffset >= paddleEdge))
   && (y `isBetween` (paddleTop, paddleBottom)
    || ly `isBetween` (paddleTop, paddleBottom)
    || paddleTop `isBetween` (y, ly)
    || paddleBottom `isBetween` (y, ly))

shouldBounceY : Model -> Ball -> Bool
shouldBounceY model ball =
  let (_,y) = ball.p
      (_,vy) = ball.v
  in
     vy < 0 && y < -(getMaxY model.canvas - ball.size)
  || vy > 0 && y > getMaxY model.canvas - ball.size

-- Adds vertical speed to the ball if it hits of center
changeY : Model -> Paddle -> Ball -> Ball
changeY model paddle ball =
  let (_,y) = ball.p
      (vx,vy) = ball.v
      maxOffset = paddle.height/2
      offset = clamp -maxOffset maxOffset (paddle.y - y)
      abssq x = x* abs x
  in { ball | v = (vx, vy - abssq (offset * 0.2)) }

bouncePaddle : Model -> Paddle -> Ball -> Ball
bouncePaddle model paddle ball =
  if shouldBouncePaddle model paddle ball then
     ball
       |> bounceX
       |> changeY model paddle
  else ball

bounceWalls : Model -> Ball -> Ball
bounceWalls model ball =
  if shouldBounceY model ball then
     bounceY model.canvas ball else
     ball

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
  let newPos = toFloat direction * getMaxY model.canvas
  in if direction /= 0 then movePaddle timeDiff model newPos paddle
     else paddle

tickPaddle : Time.Time -> Model -> Paddle -> Paddle
tickPaddle timeDiff model paddle =
  case paddle.controlledBy of
    EasyComputer -> moveAI timeDiff model 0 paddle
    HardComputer -> moveAI timeDiff model -0.5 paddle
    InsaneComputer -> moveInsaneAI timeDiff model 0.8 paddle
    MouseControls -> movePaddle timeDiff model model.mouseY paddle
    ArrowControls -> movePaddleKeyboard model timeDiff model.arrowDirection paddle
    WasdControls -> movePaddleKeyboard model timeDiff model.wasdDirection paddle

tickPaddles : Time.Time -> Model -> Model
tickPaddles timeDiff model =
  { model |
    leftPaddle = tickPaddle timeDiff model model.leftPaddle
  , rightPaddle = tickPaddle timeDiff model model.rightPaddle
  }

onTick : Time.Time -> Model -> Model
onTick timeDiff model =
  if model.state == Running then
    model
      |> moveBall timeDiff
      |> tickPaddles timeDiff
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

cyclePlayerType : Paddle -> Paddle
cyclePlayerType paddle =
  { paddle |
    controlledBy =
    case paddle.controlledBy of
      MouseControls -> ArrowControls
      ArrowControls -> WasdControls
      WasdControls -> EasyComputer
      EasyComputer -> HardComputer
      HardComputer -> InsaneComputer
      InsaneComputer -> MouseControls
  }

changeLeftPlayer : Model -> Model
changeLeftPlayer model =
  { model | leftPaddle = cyclePlayerType model.leftPaddle}

changeRightPlayer : Model -> Model
changeRightPlayer model =
  { model | rightPaddle = cyclePlayerType model.rightPaddle }

--- Update ---
update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  (case action of
    NoOp -> model
    MouseMove pos -> onMouseMove model pos
    ArrowPressed dir -> onArrowPressed model dir
    WasdPressed dir -> onWasdPressed model dir
    SpacePressed pressed -> toggleState pressed model
    Tick time -> onTick time model
    ChangeLeftPlayer -> changeLeftPlayer model
    ChangeRightPlayer -> changeRightPlayer model
  , Cmd.none)

---- Keyboard handling -----
upArrow : Keyboard.KeyCode
upArrow = 38
downArrow : Keyboard.KeyCode
downArrow = 40
leftArrow : Keyboard.KeyCode
leftArrow = 37
rightArrow : Keyboard.KeyCode
rightArrow = 39

keyDown : Model -> Keyboard.KeyCode -> Msg
keyDown model code =
  if code == Char.toCode ' ' then
     SpacePressed True else
  if code == Char.toCode 'W' then
     WasdPressed 1 else
  if code == Char.toCode 'S' then
     WasdPressed -1 else
  if code == upArrow then
     ArrowPressed 1 else
  if code == downArrow then
     ArrowPressed -1 else
  if code == leftArrow then
     ChangeLeftPlayer else
  if code == rightArrow then
     ChangeRightPlayer else
     NoOp

keyUp : Model -> Keyboard.KeyCode -> Msg
keyUp model code =
  if code == Char.toCode ' ' then
     SpacePressed False else
  if code == Char.toCode 'W' && model.wasdDirection == 1 then
     WasdPressed 0 else
  if code == Char.toCode 'S' && model.wasdDirection == -1 then
     WasdPressed 0 else
  if code == upArrow && model.arrowDirection == 1 then
     ArrowPressed 0 else
  if code == downArrow && model.arrowDirection == -1 then
     ArrowPressed 0 else
     NoOp

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
canvas canvas =
  collage canvas.width canvas.height
    >> color canvas.color

scoreText : Color -> String -> Element
scoreText color =
  Text.fromString
    >> Text.height 30
    >> Text.monospace
    >> Text.color color
    >> leftAligned

playerTypeText : Color -> String -> Element
playerTypeText color =
  Text.fromString
    >> Text.height 10
    >> Text.monospace
    >> Text.bold
    >> Text.color color
    >> centered

scoreColor : Int -> Int -> Color
scoreColor myScore theirScore =
  if myScore > theirScore then green
  else if myScore < theirScore then red
  else yellow

paddleScore : Paddle -> Color -> Element
paddleScore paddle color =
  scoreText color ( paddle.name ++ ": " ++ toString paddle.score)

playerType : Paddle -> Element
playerType paddle =
  playerTypeText green <| case paddle.controlledBy of
    MouseControls -> "Mouse"
    ArrowControls -> "Keyboard arrows"
    WasdControls -> "Keyboard wasd"
    EasyComputer -> "Computer - Easy"
    HardComputer -> "Computer - Hard"
    InsaneComputer -> "Computer - Insane"

score : Model -> Element
score model =
  let canvasWidth = model.canvas.width
      scoreHeight = model.scoreHeight
      leftColor = scoreColor model.leftPaddle.score model.rightPaddle.score
      rightColor = scoreColor model.rightPaddle.score model.leftPaddle.score
      scoreContainer = container (canvasWidth//2) (scoreHeight//2+10)  midBottom
      playerTypeContainer = container (canvasWidth//2) (scoreHeight//2-10) midTop
  in flow right
      [ flow down
        [ paddleScore model.leftPaddle leftColor
            |> scoreContainer
        , playerType model.leftPaddle
            |> playerTypeContainer
        ]
      , flow down
        [ paddleScore model.rightPaddle rightColor
            |> scoreContainer
        , playerType model.rightPaddle
            |> playerTypeContainer
        ]
      ]
     |> container canvasWidth scoreHeight middle
     |> color black

pauseMessageText : String -> Element
pauseMessageText =
  Text.fromString
    >> Text.height 12
    >> Text.monospace
    >> Text.color white
    >> centered

pauseMessage : Model -> Form
pauseMessage model =
    toForm
    <| if model.state == Paused then
       flow down
       [ pauseMessageText "Space to pause/unpause."
       , pauseMessageText "Left/Right arrow to change controls."
       ]
       else empty

view : Model -> Html.Html Msg
view model =
  toHtml <| flow down
    [ canvas model.canvas
        [ paddle model.leftPaddle
        , paddle model.rightPaddle
        , shadowBall model.ball -- gray previous position of ball to see movement when paused.
        , ball model.ball
        , pauseMessage model
        ]
    , spacer model.canvas.width 1
    , score model
    ]



----- Subscriptions -----

subscriptions : Model -> Sub Msg
subscriptions model =
  Platform.Sub.batch
  [ Keyboard.downs (keyDown model)
  , Keyboard.ups (keyUp model)
  , AnimationFrame.diffs Tick
  , Mouse.moves (\{x,y} -> MouseMove (x,y))
  ]

main : Program Never
main = Html.App.program
  {
    init = (model0, Cmd.none),
    update = update,
    subscriptions = subscriptions,
    view = view
  }
