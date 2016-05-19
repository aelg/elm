
import List
import Html exposing (Html)
import Html.App
import Html.Attributes
import Html.Events
import Json.Decode as Decode exposing (Decoder, (:=))
import Http
import Task exposing (Task)


-- Model
type Status = 
  Ok
  | Error Http.Error


type Msg = 
  HttpError Http.Error
  | NewData Data
  | Button1
  | Button2
  | Button3
  | Button4


type alias Data = 
  { aString : String
  , aNumber : Int
  , aFloat : Float
  , aList : List Int
  }


type alias Model = 
  { data : Data
  , status : Status
  }


data0 = Data "initial" 0 0 []
model0 = Model data0 Ok


-- Json Decoding
dataDecoder : Decoder Data
dataDecoder = 
  let decode aString aNumber aFloat aList = 
    { aString = aString
    , aNumber = aNumber
    , aFloat = aFloat 
    , aList = aList
    }
  in
    Decode.object4 decode
      ( "aString" := Decode.string )
      ( "aNumber" := Decode.int )
      ( "aFloat" := Decode.float )
      ( "aList" := Decode.list Decode.int )

getData : String -> Cmd Msg
getData = 
  let task = Http.get dataDecoder
  in Task.perform HttpError NewData << task 


-- Update
update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    HttpError error -> 
      ({ model | status = Error error }, Cmd.none)

    NewData data -> 
      ({ model | data = data, status = Ok }, Cmd.none)

    Button1 -> 
      (model, getData "/data1.json")

    Button2 -> 
      (model, getData "/data2.json")

    Button3 -> 
      (model, getData "/data3.json")

    Button4 -> 
      (model, getData "/non-existing.json")


-- View
leftMargin : Html.Attribute Msg
leftMargin = Html.Attributes.style [("marginLeft", "20px")]

topMargin : Html.Attribute Msg
topMargin = Html.Attributes.style [("marginTop", "20px")]

red : Html.Attribute Msg
red = Html.Attributes.style [("color", "red")]

green : Html.Attribute Msg
green = Html.Attributes.style [("color", "green")]

emptyRow : Html Msg
emptyRow = Html.br [] []

data : Data -> Html Msg
data data = 
  Html.div [] 
    [ Html.text <| "Data:"
    , Html.div [ leftMargin ]
      [ Html.div [] [ Html.text <| "aString: " ++ data.aString ]
      , Html.div [] [ Html.text <| "aNumber: " ++ toString data.aNumber ]
      , Html.div [] [ Html.text <| "aFloat: " ++ toString data.aFloat ]
      , Html.div [] [ Html.text <| "aList: " ++ toString data.aList ]
      ]
    ]


parseError : Http.Error -> String
parseError error = 
  case error of
    Http.Timeout -> "Timeout"
    Http.NetworkError -> "NetworkError"
    Http.UnexpectedPayload payload -> "Unexpected payload: " ++ payload
    Http.BadResponse code message ->  toString code ++ " " ++ message


status : Status -> Html Msg
status status = 
  case status of
    Ok -> 
      Html.div [ green ] [ Html.text "Ok" ]

    Error error -> 
      Html.div [ red ] [ Html.text <| "Error: " ++ parseError error ]


buttons : Html Msg
buttons = 
  Html.div []
    [ Html.button [ Html.Events.onClick Button1 ] [ Html.text "data1.json" ]
    , Html.button [ Html.Events.onClick Button2 ] [ Html.text "data2.json" ]
    , Html.button [ Html.Events.onClick Button3 ] [ Html.text "data3.json" ]
    , Html.button [ Html.Events.onClick Button4 ] [ Html.text "non-existing.json" ]
    ]


view : Model -> Html Msg
view model = 
  Html.div [topMargin, leftMargin] 
    [ buttons
    , status model.status
    , emptyRow
    , data model.data
    ]


-- Main
main : Program Never
main = Html.App.program 
  { init = (model0, getData "/data1.json")
  , update = update
  , view = view
  , subscriptions = \_ -> Sub.none
  } 
