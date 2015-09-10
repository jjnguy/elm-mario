import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window


-- MODEL

type alias Model =
  { x : Float
  , y : Float
  , z : Float
  , vx : Float
  , vy : Float
  , vz : Float
  , dir : Direction
  }


type Direction = Left | Right | Farther | Closer

type alias Keys = { x:Int, y:Int }

mario : Model
mario =
  { x = 0
  , y = 0
  , z = 0
  , vx = 0
  , vy = 0
  , vz = 0
  , dir = Right
  }


-- UPDATE

update : (Float, Keys, Keys) -> Model -> Model
update (dt, arrow, wasd) mario =
  mario
    |> gravity dt
    |> jump arrow wasd
    |> walk arrow wasd
    |> physics dt


jump : Keys -> Keys -> Model -> Model
jump arrow wasd mario =
  if arrow.y > 0 && mario.vy == 0
    then { mario | vy <- 6.0 }
    else mario


gravity : Float -> Model -> Model
gravity dt mario =
  { mario |
      vy <- if mario.y > 0 then mario.vy - dt/4 else 0
  }


physics : Float -> Model -> Model
physics dt mario =
  { mario |
      x <- mario.x + dt * mario.vx,
      z <- mario.z + dt * mario.vz,
      y <- max 0 (mario.y + dt * mario.vy)
  }


walk : Keys -> Keys -> Model -> Model
walk arrow wasd mario =
  { mario |
      vx <- toFloat arrow.x,
      vz <- toFloat wasd.y,
      dir <-
        if  | arrow.x < 0 -> Left
            | arrow.x > 0 -> Right
            | otherwise  -> mario.dir
  }


-- VIEW

view : (Int, Int) -> Model -> Element
view (w',h') mario =
  let
    (w,h) = (toFloat w', toFloat h')

    verb =
      if  | mario.y  >  0 -> "jump"
          | mario.vx /= 0 || mario.vz /= 0 -> "walk"
          | otherwise     -> "stand"

    dir =
      case mario.dir of
        Left -> "left"
        Right -> "right"

    src =
      "imgs/mario/"++ verb ++ "/" ++ dir ++ ".gif"

    marioImage =
      image 35 35 src

    groundY = 62 - h/2

    position =
      (mario.x, mario.y + groundY + mario.z)
  in
    collage w' h'
      [ rect w h
          |> filled (rgb 174 238 238)
      , rect w 100
          |> filled (rgb 74 167 43)
          |> move (0, 24 - h/2)
      , marioImage
          |> toForm
          |> move position
      ]


-- SIGNALS

main : Signal Element
main =
  Signal.map2 view Window.dimensions (Signal.foldp update mario input)

input : Signal (Float, Keys, Keys)
input =
  let
    delta = Signal.map (\t -> t/20) (fps 30)
  in
    Signal.sampleOn delta (Signal.map3 (,,) delta Keyboard.arrows Keyboard.wasd)
