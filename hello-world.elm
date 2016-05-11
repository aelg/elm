module Hello where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events
import Json.Decode
import String

fib : {a | fib1 : Int, fib2 : Int} -> Int ->  Int
fib m a = 
  case a of
  1 -> m.fib1
  2 -> m.fib2
  _ -> fib m (a-1) + fib m (a-2)


outline : List (String, String)
outline =
  [ ("border", "1px solid black") ]


margin : Int -> List (String, String)
margin n =
  [ ("margin", String.concat [n |> toString, "px"]) ]

rowNumberAndFunc func n =
  tr [] [
    td [style outline] [text (toString n)],
    td [style outline] [text (toString (func n))]
  ]

headerRow : String -> String -> Html
headerRow a b =
  tr []
  [ th [style outline] [text a]
  , th [style outline] [text b]
  ]


type Action = 
  NoOp
  | UpdateRows String 
  | UpdateFib1 String 
  | UpdateFib2 String
  | SetFibonacci
  | SetLucas


message : Signal.Mailbox Action
message =
  Signal.mailbox 
    NoOp


update : Action -> Model -> Model
update action model =
  case action of
    NoOp -> model
    UpdateRows rawString ->
      case (String.toInt rawString) of
        Ok a -> { model | rows = a }
        Err _ -> model
    UpdateFib1 rawString ->
      case (String.toInt rawString) of
        Ok a -> { model | fib1 = a }
        Err _ -> model
    UpdateFib2 rawString ->
      case (String.toInt rawString) of
        Ok a -> { model | fib2 = a }
        Err _ -> model
    SetFibonacci -> {model | fib1 = 1, fib2 = 1}
    SetLucas -> {model | fib1 = 1, fib2 = 3}


onChange : Signal.Address Action -> (String -> Action) -> Attribute
onChange address action =
  Html.Events.on "change" Html.Events.targetValue (\str -> Signal.message address (action str))


onClick : Signal.Address Action -> Action -> Attribute
onClick address action =
  Html.Events.on "click" Json.Decode.value (\_ -> Signal.message address action)


type alias Model =
  { fib1 : Int
  , fib2 : Int
  , rows : Int
  }


range : Int -> List Int
range n =
  case n of
    0 -> []
    _ -> (range (n-1)) ++ [n]


fibonacciTable : Model -> Html
fibonacciTable model = 
  table []
    ( [headerRow "N" "Value"] ++
      List.map (rowNumberAndFunc (fib model)) (range model.rows)
    )

view : Model -> Html
view model =
    Html.div [style (margin 50)]
      [ input [type' "button", onClick message.address SetFibonacci, value "Fibonacci"] []
      , input [type' "button", onClick message.address SetLucas, value "Lucas"] []
      , br [] []
      , br [] []
      , table [] 
        [ tr [] 
          [ td [] [ text "1: " ]
          , td [] 
            [ input 
              [type' "number"
              , value (toString model.fib1)
              , onChange message.address UpdateFib1
              ] []
            ]
          ]
        , tr [] 
          [ td [] [ text "2: "]
          , td [] 
            [ input 
              [type' "number"
              , value (toString model.fib2)
              , onChange message.address UpdateFib2
              ] []
            ]
          ]
        , tr [] 
          [ td [] [ text "Rows: "]
          , td [] 
            [ input 
              [type' "number"
              , value (toString model.rows)
              , onChange message.address UpdateRows
              ] []
            ]
          ]
        ]
      , br [] []
      , fibonacciTable model
      ]


model : Signal.Signal Model
model = 
  Signal.foldp update (Model 1 1 10) message.signal


main : Signal.Signal Html
main =
  Signal.map view model

