module Main exposing (..)

import Html exposing (..)
import Material
import Material.Slider as Slider
import Material.Grid exposing (grid, cell, size, Device(..))


-- MODEL


type alias Model =
    { count : Int
    , blocksize : Int
    , avgTxSize : Int
    , mdl :
        Material.Model
        -- Boilerplate: model store for any and all Mdl components you use.
    }


model : Model
model =
    { count = 0
    , blocksize = 1000000
    , avgTxSize = 500
    , mdl =
        Material.model
        -- Boilerplate: Always use this initial Mdl model store.
    }

-- ACTION, UPDATE


type Msg
    = BlocksizeChange Float
    | Mdl (Material.Msg Msg)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BlocksizeChange value ->
            ( { model | blocksize = round value }
            , Cmd.none
            )

        -- Boilerplate: Mdl action handler.
        Mdl msg_ ->
            Material.update Mdl msg_ model

-- VIEW

txPerBlock : Model -> Int
txPerBlock model = round (toFloat model.blocksize / toFloat model.avgTxSize)

blocksPerDay : Int
blocksPerDay = round (24 * 60 / 10.0)

txPerDay : Model -> Int
txPerDay model = blocksPerDay * txPerBlock model

type alias Mdl =
    Material.Model

view : Model -> Html Msg
view model =
    grid []
      [ cell [ size All 4 ]
          [ h5 [] [ text "Block settings" ]
          , Slider.view
              [ Slider.onChange BlocksizeChange
              , Slider.value (toFloat model.blocksize)
              , Slider.max 32000000
              , Slider.min 1000000
              , Slider.step 1000000
              ]
          , p [] [ text ("Block size: " ++ toString model.blocksize)]
          , p [] [ text ("Average transaction size: " ++ toString model.avgTxSize)]
          , p [] [ text ("Transactions per block : " ++ toString (txPerBlock model))]
          , p [] [ text ("Blocks per day : " ++ toString blocksPerDay)]
          , p [] [ text ("Transactions per day : " ++ toString (txPerDay model))]
          ]
      ]

main : Program Never Model Msg
main =
    Html.program
        { init = ( model, Cmd.none )
        , view = view
        , subscriptions = always Sub.none
        , update = update
        }
