import Html
import Html.Attributes
import Html.App
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Color exposing (..)
import Time
import Mouse
import Keyboard
import AnimationFrame
import Platform.Sub
import Char
import Random

----- Model -----
type alias Ball =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , lastx : Float
  , lasty : Float
  , size : Float
  , color : Color
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

newBall =
  { x = -320
  , y = 0
  , minVx = 300
  , maxVx = 400
  , maxVy = 200
  }

model0 : Model
model0 =
  { canvas = Canvas 800 500 black
  , scoreHeight = 100
  , leftPaddle = Paddle "Left Player" 10 100 -360 0 0 white WasdControls 200
  , rightPaddle = Paddle "Right Player" 10 100 360 0 0 white EasyComputer 200
  , ball = ball0
  , score = (0, 0)
  , speedIncrease = 10
  , state = Paused
  , mouseY = 0
  , arrowDirection = 0
  , wasdDirection = 0
  }

ball0 : Ball
ball0 =
  { x = 0
  , y = 0
  , vx = 0
  , vy = 0
  , lastx = 0
  , lasty = 0
  , size = 10
  , color = white
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
  | NewBall Ball

--- Utils ---
{--
  Apply function on condition:
  `cond ? func <| a` is the same as
  if cond then
     func a else
     a
--}
(?) : Bool -> (a -> a) -> a -> a
(?) doIt f = if doIt then f else identity
infixl 1 ?

-- For calculating where the ball is after it bounces.
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
movePaddle : Time.Time -> Model -> Float -> Paddle -> Paddle
movePaddle timeDiff model wantedY paddle =
  let diff = wantedY - paddle.y
      maxDiff = paddle.speed * Time.inSeconds timeDiff
      restrictedDiff = clamp (-maxDiff) maxDiff diff
      newY = paddle.y + restrictedDiff
      maxY = getMaxY model.canvas - paddle.height/2
  in { paddle | y = clampFold -maxY maxY newY }

--- Input Handling ---
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

--- Computer movement ---

getAggressivenessOffset ball paddle aggressiveness =
  if ball.vy < 0 then
     (paddle.height/2) * aggressiveness else
    -(paddle.height/2) * aggressiveness

type alias ComputerAlgorithm = Model -> Paddle -> Float
insaneComputer : Float -> Model -> Paddle -> Float
insaneComputer aggressiveness model paddle =
  let ball = model.ball
      toward = ball.x < paddle.x && ball.vx > 0 || ball.x > paddle.x && ball.vx < 0
      aggressivenessOffset = getAggressivenessOffset ball paddle aggressiveness
      sizeOffset = model.ball.size + paddle.width/2
      dY = ball.vy / abs ball.vx
      paddleToBall = abs (ball.x - paddle.x) - sizeOffset
      fieldWidth = (abs (model.leftPaddle.x - model.rightPaddle.x) - sizeOffset)
      deltaX =
        if toward then
           paddleToBall else
           2*fieldWidth - paddleToBall
      maxY = getMaxY model.canvas - model.ball.size
  in clampFold -maxY maxY (dY * deltaX + ball.y + aggressivenessOffset)

normalComputer : Float -> Model -> Paddle -> Float
normalComputer aggressiveness model paddle =
  let ball = model.ball
      toward = ball.x < paddle.x && ball.vx > 0 || ball.x > paddle.x && ball.vx < 0
      aggressivenessOffset = getAggressivenessOffset ball paddle aggressiveness
      restPos = if ball.vy > 0 then
                   getMaxY model.canvas/2 else
                   -(getMaxY model.canvas/2)
  in if toward then
        ball.y + aggressivenessOffset else
        restPos

moveComputer : Time.Time -> Model -> ComputerAlgorithm -> Paddle -> Paddle
moveComputer timeDiff model ai paddle =
  let aiPos = ai model paddle
  in movePaddle timeDiff model aiPos paddle

--- Ball movement ---
moveBall : Time.Time -> Model -> Model
moveBall timeDiff model =
  let ball = model.ball
      ball' = { ball |
                x = ball.x + ball.vx * Time.inSeconds timeDiff
              , y = ball.y + ball.vy * Time.inSeconds timeDiff
              , lastx = ball.x
              , lasty = ball.y
              }
  in { model | ball = ball' }

bounceX : Ball -> Ball
bounceX ball =
  { ball | vx = -ball.vx }

bounceY : Canvas -> Ball -> Ball
bounceY canvas ball =
  let maxY = getMaxY canvas - ball.size
  in { ball |
       vy = negate ball.vy
     , y = clampFold -maxY maxY ball.y
  }

shouldBouncePaddle : Model -> Paddle -> Ball -> Bool
shouldBouncePaddle model paddle ball =
  let paddleEdge = if ball.vx > 0
        then paddle.x - paddle.width/2
        else paddle.x + paddle.width/2
      ballEdgeOffset = if ball.vx > 0
        then ball.size
        else -ball.size
      ballEdge = ball.x + ballEdgeOffset
      lastBallEdge = ball.lastx + ballEdgeOffset
      paddleTop = paddle.y + paddle.height/2
      paddleBottom = paddle.y - paddle.height/2
      isBetween a (x, y) = a >= x && a <= y || a >= y && a <= x
  in paddleEdge `isBetween` (ballEdge, lastBallEdge) &&
     ( ball.y `isBetween` (paddleTop, paddleBottom) ||
       ball.lasty `isBetween` (paddleTop, paddleBottom) ||
       paddleTop `isBetween` (ball.y, ball.lasty) ||
       paddleBottom `isBetween` (ball.y, ball.lasty))

shouldBounceY : Model -> Ball -> Bool
shouldBounceY model ball =
  ball.vy < 0 && ball.y < -(getMaxY model.canvas - ball.size) ||
  ball.vy > 0 && ball.y > getMaxY model.canvas - ball.size

-- Calculates vertical speed change when the ball hits the paddle of center
bounceSpeedChangeY : Paddle -> Ball -> Float
bounceSpeedChangeY paddle ball =
  let maxOffset = paddle.height/2
      offset = clamp -maxOffset maxOffset (paddle.y - ball.y)
      abssq x = x* abs x
  in negate <| abssq (offset * 0.2)

bouncePaddle : Model -> Paddle -> Ball -> Ball
bouncePaddle model paddle ball =
  if shouldBouncePaddle model paddle ball then
     let x' = clampFold model.leftPaddle.x model.rightPaddle.x ball.x
         vx' = -ball.vx
         vy' = ball.vy + bounceSpeedChangeY paddle ball
     in { ball | x = x', vx = vx', vy = vy' } else
     ball

bounceWalls : Model -> Ball -> Ball
bounceWalls model ball =
  shouldBounceY model ball ? bounceY model.canvas <| ball

ballCollision : Model -> Model
ballCollision model =
  {model | ball =
    model.ball
      |> bouncePaddle model model.leftPaddle
      |> bouncePaddle model model.rightPaddle
      |> bounceWalls model
  }

makeNewBall : Cmd Msg
makeNewBall =
  let ball0 = model0.ball
      x = newBall.x
      y = newBall.y
      vx = Random.float (newBall.minVx) (newBall.maxVx)
      vy = Random.float -newBall.maxVy newBall.maxVy
      shouldMirror = Random.bool
      toBall (doMirror, (vx, vy)) =
        doMirror ? mirror <| { ball0 | x = x, y = y
                             , vx = vx, vy = vy
                             , lastx = x, lasty = y
                             }
      mirror ball = { ball | x = -ball.x, vx = -ball.vx, lastx = -ball.lastx}
      randomBall = Random.pair shouldMirror (Random.pair vx vy)
  in Random.generate NewBall (Random.map toBall randomBall)

gameLost : Model -> (Model, Cmd Msg)
gameLost model =
  let ball = model.ball
      increaseScore paddle = { paddle | score = paddle.score + 1 }
  in if ball.x < -(getMaxX model.canvas) then
        { model | rightPaddle = increaseScore model.rightPaddle } ! [makeNewBall]
     else if ball.x > getMaxX model.canvas then
        { model | leftPaddle = increaseScore model.leftPaddle } ! [makeNewBall]
     else (model, Cmd.none)

increaseSpeed : Time.Time -> Model -> Model
increaseSpeed timeDiff model =
  let ball = model.ball
      vxSign = ball.vx/abs ball.vx
      speedIncrease = model.speedIncrease * vxSign * Time.inSeconds timeDiff
  in { model | ball =
       { ball | vx =
         ball.vx + speedIncrease}}

movePaddleKeyboard : Model -> Time.Time -> Int -> Paddle -> Paddle
movePaddleKeyboard model timeDiff direction paddle =
  let newPos = toFloat direction * getMaxY model.canvas
  in direction /= 0 ? movePaddle timeDiff model newPos <| paddle

tickPaddle : Time.Time -> Model -> Paddle -> Paddle
tickPaddle timeDiff model paddle =
  case paddle.controlledBy of
    EasyComputer -> moveComputer timeDiff model (normalComputer 0) paddle
    HardComputer -> moveComputer timeDiff model (normalComputer -0.5) paddle
    InsaneComputer -> moveComputer timeDiff model (insaneComputer 0.7) paddle
    MouseControls -> movePaddle timeDiff model model.mouseY paddle
    ArrowControls -> movePaddleKeyboard model timeDiff model.arrowDirection paddle
    WasdControls -> movePaddleKeyboard model timeDiff model.wasdDirection paddle

tickPaddles : Time.Time -> Model -> Model
tickPaddles timeDiff model =
  { model |
    leftPaddle = tickPaddle timeDiff model model.leftPaddle
  , rightPaddle = tickPaddle timeDiff model model.rightPaddle
  }

onTick : Time.Time -> Model -> (Model, Cmd Msg)
onTick timeDiff model =
  if model.state == Running then
     model
       |> moveBall timeDiff
       |> tickPaddles timeDiff
       |> ballCollision
       |> increaseSpeed timeDiff
       |> gameLost
  else (model, Cmd.none)

toggleState : Model -> Model
toggleState model =
  { model | state =
     case model.state of
       Running -> Paused
       Paused  -> Running
  }

cyclePlayerType : Paddle -> Paddle
cyclePlayerType paddle =
  { paddle | controlledBy =
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
update msg model =
  case msg of
    NoOp -> (model, Cmd.none)
    MouseMove pos -> onMouseMove model pos ! [Cmd.none]
    ArrowPressed dir -> onArrowPressed model dir ! [Cmd.none]
    WasdPressed dir -> onWasdPressed model dir ! [Cmd.none]
    SpacePressed pressed -> (pressed ? toggleState <| model) ! [Cmd.none]
    Tick time -> onTick time model
    ChangeLeftPlayer -> changeLeftPlayer model ! [Cmd.none]
    ChangeRightPlayer -> changeRightPlayer model ! [Cmd.none]
    NewBall newBall -> { model | ball = newBall } ! [Cmd.none]

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

paddle : Paddle -> Svg Msg
paddle paddle =
  rect
    [ x (toString <| paddle.x - paddle.width/2)
    , y (toString <| -paddle.y - paddle.height/2)
    , width (toString paddle.width)
    , height (toString paddle.height)
    , colorFill paddle.color
    ]
    []

ball : Ball -> Svg Msg
ball ball =
  circle
    [ cx (toString ball.x)
    , cy (toString -ball.y)
    , r (toString ball.size)
    , colorFill white
    ]
    []

shadowBall : Ball -> Svg Msg
shadowBall ball =
  circle
    [ cx (toString ball.lastx)
    , cy (toString -ball.lasty)
    , r (toString ball.size)
    , colorFill white
    , fillOpacity "0.5"
    ]
    []

background : Svg Msg
background =
  rect
    [ width "100%"
    , height "100%"
    , colorFill black
    ]
    []

colorFill : Color -> Svg.Attribute Msg
colorFill color =
  let rgb = toRgb color
  in
    fill <|
      "rgb(" ++ toString rgb.red ++
      ", " ++ toString rgb.green ++
      ", " ++ toString rgb.blue ++
      ")"

translate : Int -> Int -> Svg.Attribute Msg
translate x y =
  transform ("translate(" ++ toString x ++ " " ++ toString y ++ ")")

canvas : (Int, Int) -> List (Svg Msg) -> Html.Html Msg
canvas (canvasWidth, canvasHeight) elements =
  svg [ width (toString canvasWidth)
      , height (toString canvasHeight)
      , fill "black"
      ]
      [ background
      , g [ translate (canvasWidth//2) (canvasHeight//2) ] elements
      ]

scoreText : Color -> String -> Svg Msg
scoreText color text =
  tspan
    [ fontSize "30"
    , fontFamily "monospace"
    , textAnchor "middle"
    , colorFill color
    , y "-1.6em"
    , dy "1.6em"
    ]
    [ Svg.text text ]

playerTypeText : Color -> String -> Svg Msg
playerTypeText color text =
  tspan
    [ fontSize "10"
    , fontFamily "monospace"
    , fontWeight "bold"
    , textAnchor "middle"
    , colorFill color
    , dy "1.6em"
    , x "0"
    ]
    [ Svg.text text ]

scoreColor : Int -> Int -> Color
scoreColor myScore theirScore =
  if myScore > theirScore then green
  else if myScore < theirScore then red
  else yellow

paddleScore : Paddle -> Color -> Svg Msg
paddleScore paddle color =
  scoreText color ( paddle.name ++ ": " ++ toString paddle.score)

playerType : Paddle -> Svg Msg
playerType paddle =
  playerTypeText green <| case paddle.controlledBy of
    MouseControls -> "Mouse"
    ArrowControls -> "Keyboard arrows"
    WasdControls -> "Keyboard wasd"
    EasyComputer -> "Computer - Easy"
    HardComputer -> "Computer - Hard"
    InsaneComputer -> "Computer - Insane"

score : Model -> Svg Msg
score model =
  let leftColor = scoreColor model.leftPaddle.score model.rightPaddle.score
      rightColor = scoreColor model.rightPaddle.score model.leftPaddle.score
      leftPos = -model.canvas.width//4
      rightPos =  model.canvas.width//4
  in canvas (model.canvas.width, model.scoreHeight)
       [ text' [ translate leftPos 0 ]
           [ paddleScore model.leftPaddle leftColor
           , playerType model.leftPaddle
           ]
       , text' [ translate rightPos 0 ]
           [ paddleScore model.rightPaddle rightColor
           , playerType model.rightPaddle
           ]
       ]

pauseMessageText : String -> Svg Msg
pauseMessageText text =
  tspan [ fontSize "12"
        , fontFamily "monospace"
        , textAnchor "middle"
        , colorFill white
        , dy "1.6em"
        , x "0"
        ]
        [ Svg.text text ]

pauseMessage : State -> Svg Msg
pauseMessage state =
     text' [] <|
       if state == Paused then
          [ pauseMessageText "Space to pause/unpause."
          , pauseMessageText "Left/Right arrow to change controls."
          ]
       else
          []

game : Model -> Html.Html Msg
game model =
  canvas (model.canvas.width, model.canvas.height)
    [ background
    , paddle model.leftPaddle
    , paddle model.rightPaddle
    , shadowBall model.ball -- gray previous position of ball to see movement when paused.
    , ball model.ball
    , pauseMessage model.state
    ]

view : Model -> Html.Html Msg
view model =
  Html.div
    [ Html.Attributes.style [ ("width", toString model.canvas.width ++ "px") ]
    ]
    [ game model
    , score model
    ]


----- Subscriptions -----

subscriptions : Model -> Sub Msg
subscriptions model =
  if model.state == Running then
    Platform.Sub.batch
    [ Keyboard.downs (keyDown model)
    , Keyboard.ups (keyUp model)
    , AnimationFrame.diffs Tick
    , Mouse.moves (\{x,y} -> MouseMove (x,y))
    ]
    else
    Platform.Sub.batch
    [ Keyboard.downs (keyDown model)
    , Keyboard.ups (keyUp model)
    ]


main : Program Never
main = Html.App.program
  { init = (model0, makeNewBall)
  , update = update
  , subscriptions = subscriptions
  , view = view
  }
