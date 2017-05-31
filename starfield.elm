import Html exposing (Html, div, text)
import Time exposing (Time, second)

-- Main
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- Model
type alias Model = Star

type alias Star = {
  x: Int
  , y: Int
  , ratio: Float
}

init: (Model, Cmd Msg)
init =
  ({x = 0, y = 0, ratio = 0.1}, Cmd.none)


-- Update
type Msg = Reset |
  Increment Time

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Reset ->
      ({ x = 0, y = 0, ratio = 0.1 }, Cmd.none)
    Increment newTime ->
      ({ x = model.x, y = model.y, ratio = model.ratio + 0.2 }, Cmd.none)


-- View
view: Model -> Html Msg
view model =
  div [] [
    text (toString model)
  ]



-- Subscriptions
subscriptions: Model -> Sub Msg
subscriptions model =
  Time.every second Increment
