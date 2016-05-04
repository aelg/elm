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
    td [] [text (toString n)],
    td [] [text (toString (func n))]
  ]

headerRow : String -> String -> Html
headerRow a b =
  tr []
  [ th [style outline] [text a]
  , th [style outline] [text b]
  ]


message : Signal.Mailbox Model
message =
  Signal.mailbox 
    { fib1 = 1
    , fib2 = 1
    , rows = 10
    }


update : Model -> Action -> Model
update model action =
  case action of
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


onChange : Signal.Address Model -> Model -> (String -> Action) -> Attribute
onChange address model action =
  Html.Events.on "change" Html.Events.targetValue (\str -> Signal.message address (update model (action str)))


onClick : Signal.Address Model -> Model -> Action -> Attribute
onClick address model action =
  Html.Events.on "click" Json.Decode.value (\_ -> Signal.message address (update model action))


type alias Model =
  { fib1 : Int
  , fib2 : Int
  , rows : Int
  }

type Action = 
  UpdateRows String 
  | UpdateFib1 String 
  | UpdateFib2 String
  | SetFibonacci
  | SetLucas


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

view : Signal.Address Model -> Model -> Html
view message model =
    Html.div [style (margin 50)]
      [ input [type' "button", onClick message model SetFibonacci, value "Fibonacci"] []
      , input [type' "button", onClick message model SetLucas, value "Lucas"] []
      , br [] []
      , br [] []
      , table [] 
        [ tr [] 
          [ td [] [ text "1: " ]
          , td [] 
            [ input 
              [type' "number"
              , value (toString model.fib1)
              , onChange message model UpdateFib1
              ] []
            ]
          ]
        , tr [] 
          [ td [] [ text "2: "]
          , td [] 
            [ input 
              [type' "number"
              , value (toString model.fib2)
              , onChange message model UpdateFib2
              ] []
            ]
          ]
        , tr [] 
          [ td [] [ text "Rows: "]
          , td [] 
            [ input 
              [type' "number"
              , value (toString model.rows)
              , onChange message model UpdateRows
              ] []
            ]
          ]
        ]
      , br [] []
      , fibonacciTable model
      ]


main : Signal.Signal Html
main =
  Signal.map (view message.address) message.signal

