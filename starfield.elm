import Html exposing (Html, div, text)
import Time exposing (Time, millisecond)
import Svg exposing (svg, rect, circle)
import Svg.Attributes exposing (..)
import Random
import Basics exposing (floor)

-- Main
main: Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- Model
type alias Model = {
  stars: List Star
  , delay: Float
}

type alias Star = {
  x: Int
  , y: Int
  , ratio: Float
}

init: (Model, Cmd Msg)
init =
  ({stars = [{x = 150, y = 250, ratio = 0.1}], delay = 0.02}, Cmd.none)

randomStar : Random.Generator Star
randomStar =
  Random.map3 Star (Random.int 0 400) (Random.int 0 400) (Random.float 0.0 1.0)


-- Update
type Msg = Reset
  | Tick Time
  | GenerateStar Star


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Reset ->
      init
    Tick newTime ->
      ({stars = (List.map (incrementStars(model.delay)) model.stars), delay = model.delay}, (generateStarMessage (List.length model.stars)))
    GenerateStar star ->
      ({stars = (List.concat [model.stars, [star]]), delay = model.delay}, Cmd.none)


translateCoord : Int -> Float -> Int
translateCoord coord ratio =
  floor(toFloat(coord - 200) * ratio) + 200


incrementStars : Float -> Star -> Star
incrementStars delay star =
  {x = star.x, y = star.y, ratio = (incrementRatio star.ratio delay)}


incrementRatio : Float -> Float -> Float
incrementRatio ratio delay =
  if (ratio + delay) > 2.0 then
    0.1
  else
    ratio + delay


generateStarMessage : Int -> Cmd Msg
generateStarMessage length =
  if length < 100 then
    Random.generate GenerateStar randomStar
  else
    Cmd.none


-- View
view: Model -> Html Msg
view model =
  div [] [
    svg
    [ width "400", height "400", viewBox "0 0 400 400" ]
    (drawField model.stars)
  ]


drawField: List Star -> List (Svg.Svg msg)
drawField stars =
  List.concat [
    [ rect [x "0", y "0", width "400", height "400", fill "#000000"] [] ]
    , (List.map drawStars stars)
  ]


drawStars: Star -> Svg.Svg msg
drawStars star =
  circle [ cx(toString (translateCoord star.x star.ratio)), cy(toString (translateCoord star.y star.ratio)), r(toString(4*star.ratio)), fill "#FFFFFF"] []


-- Subscriptions
subscriptions: Model -> Sub Msg
subscriptions model =
  Time.every (1000 / 24 * millisecond) Tick
