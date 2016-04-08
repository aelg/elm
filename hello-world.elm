module Hello where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events
import Json.Decode
import String

fib: Int -> Int
fib a = 
  case a of
  1 -> 1
  2 -> 1
  _ -> fib(a-1) + fib(a-2)


outline : List (String, String)
outline =
  [ ("border", "1px solid black") ]


margin: Int -> List (String, String)
margin n =
  [ ("margin", String.concat [n |> toString, "px"]) ]

column: Int -> (Int -> Html) -> Html
column n func =
  td [ style outline ] [ func n ]


columns: List (Int -> Html) -> (Int -> Html)
columns funcs n =
  tr [] (List.map (column n) funcs)

tableBody: Int -> List Html -> (Int -> Html) -> List Html
tableBody n html rowFunc =
    case n of
      0 -> html
      _ -> (tableBody (n-1) [rowFunc n] rowFunc) ++ html


headerRow: String -> String -> Html
headerRow a b =
  tr []
  [ th [style outline] [text a]
  , th [style outline] [text b]
  ]


numberOfRows: Signal.Mailbox (Json.Decode.Decoder String)
numberOfRows =
    Signal.mailbox "0"


decodeInt: Json.Decode.Decoder String -> Int
decodeInt value =
    1

view: Signal.Address (Json.Decode.Decoder String) -> (Json.Decode.Decoder String) -> Html
view numberOfRows nStr =
  let n = decodeInt nStr in
    Html.div [style (margin 50)]
      [ input [type' "number", value (toString n), Html.Events.onClick numberOfRows Html.Events.targetValue] []
      , br [] []
      , br [] []
      , table []
        (
          headerRow "n" "Fibonacci value" ::
          tableBody n [] (columns [(toString >> text), (fib >> toString >> text)])
        )
      ]


main: Signal.Signal Html
main =
  Signal.map (view numberOfRows.address) numberOfRows.signal

