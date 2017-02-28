module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class)
import Material
import Material.Slider as Slider
import Material.Grid exposing (grid, cell, size, Device(..))


-- MODEL


type alias Model =
    { count : Int
    , blocksize : Int
    , avgTxSize : Int
    , userTxPerWeek : Int
    , mdl :
        Material.Model
        -- Boilerplate: model store for any and all Mdl components you use.
    }


model : Model
model =
    { count = 0
    , blocksize = 1000000
    , avgTxSize = 500
    , userTxPerWeek = 7
    , mdl =
        Material.model
        -- Boilerplate: Always use this initial Mdl model store.
    }

-- ACTION, UPDATE


type Msg
    = BlocksizeChange Float
    | UserTxPerWeekChange Float
    | Mdl (Material.Msg Msg)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BlocksizeChange value ->
            ( { model | blocksize = round value }
            , Cmd.none
            )

        UserTxPerWeekChange value ->
            ( { model | userTxPerWeek = round value }
            , Cmd.none
            )

        -- Boilerplate: Mdl action handler.
        Mdl msg_ ->
            Material.update Mdl msg_ model

-- VIEW

txPerBlock : Model -> Int
txPerBlock model = round (toFloat model.blocksize / toFloat model.avgTxSize)

blocksPerWeek : Int
blocksPerWeek = round (7 * 24 * 60 / 10.0)

txPerWeek : Model -> Int
txPerWeek model = blocksPerWeek * txPerBlock model

usersSupported : Model -> Int
usersSupported model = round (toFloat (txPerWeek model) / toFloat (model.userTxPerWeek))

maxGrowthPerYear : Model -> Int
maxGrowthPerYear model = blocksPerWeek * 52 * model.blocksize

type alias Mdl =
    Material.Model

view : Model -> Html Msg
view model =
    grid []
      [ cell [ size All 4 ]
          [ h5 [] [ text "Blockchain settings" ]
          , div []
              [ div [] [ text ("Block size: " ++ toString model.blocksize)]
              , Slider.view
                  [ Slider.onChange BlocksizeChange
                  , Slider.value (toFloat model.blocksize)
                  , Slider.max 32000000
                  , Slider.min 1000000
                  , Slider.step 1000000
                  ]
              , div [ class "caption" ] [ text ("Do not edit this setting if you only have a 64kbit modem ;-)")]
              ]
          ]
      , cell [ size All 4 ]
          [ h5 [] [ text "Usage settings" ]
          , Slider.view
              [ Slider.onChange UserTxPerWeekChange
              , Slider.value (toFloat model.userTxPerWeek)
              , Slider.max 1000
              , Slider.min 1
              , Slider.step 1
              ]
          , p [] [ text ("User transactions per week: " ++ toString model.userTxPerWeek)]
          , p [] [ text ("Average transaction size: " ++ toString model.avgTxSize)]
          ]
      , cell [ size All 4 ]
          [ h5 [] [ text "Bitcoin performance" ]
          , p [] [ text ("Users supported: " ++ toString (usersSupported model))]
          , p [] [ text ("Max. Blockchain growth per year: " ++ toString (maxGrowthPerYear model))]
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
