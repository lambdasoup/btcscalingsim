module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class)
import Material
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Locale)
import Material.Slider as Slider
import Material.Grid exposing (grid, cell, size, Device(..))


-- MODEL

type alias Model =
    { blocksize : Int
    , avgTxSize : Int
    , userTxPerWeek : Int
    , mdl :
        Material.Model
        -- Boilerplate: model store for any and all Mdl components you use.
    }


model : Model
model =
    { blocksize = 1000000
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

myLocale : Locale
myLocale =
    { decimals = 1
    , thousandSeparator = "'"
    , decimalSeparator = "."
    }

formatInt : Int -> String
formatInt int = format { myLocale | decimals = 0 } (toFloat int)

formatBytes : Int -> String
formatBytes int =
    if int >= 10^12 then format myLocale (toFloat int / toFloat 10^12) ++ " TB"
    else if int >= 10^9 then format myLocale (toFloat int / toFloat 10^9) ++ " GB"
    else if int >= 10^6 then format myLocale (toFloat int / toFloat 10^6) ++ " MB"
    else if int >= 10^3 then format myLocale (toFloat int / toFloat 10^3) ++ " KB"
    else toString int ++ " B"

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

dataTable : String -> String -> Html Msg -> Html Msg -> Html Msg
dataTable title caption e1 e2 =
  div []
      [ div [] [ text title ]
      , div [ class "caption" ] [ text caption]
      , table []
          [ tr []
              [ td [] [ e1 ]
              , td [] [ e2 ]
              ]
          ]
      ]

view : Model -> Html Msg
view model =
    grid []
      [ cell [ size All 4 ] (blockchainView "Blockchain settings" model)
      , cell [ size All 4 ] (usageView "Usage settings" model)
      , cell [ size All 4 ] (resultView "Bitcoin performance" model)
      ]

resultView : String -> Model -> List (Html Msg)
resultView title model =
    [ h4 [] [ text title ]
    , dataTable
      "Users supported"
      "How many users can use Bitcoin with these settings"
      ( text "")
      ( text (formatInt (usersSupported model)))
    , hr [] []
    , dataTable
      "Blockchain growth"
      "The maximum Blockchain growth per year."
      ( text "")
      ( text (formatBytes (maxGrowthPerYear model)))
    ]

usageView : String -> Model -> List (Html Msg)
usageView title model =
    [ h4 [] [ text title ]
    , dataTable
      "User transactions per week"
      "How many transaction the average Bitcoin user does per week"
      ( Slider.view
        [ Slider.onChange UserTxPerWeekChange
        , Slider.value (toFloat model.userTxPerWeek)
        , Slider.max 1000
        , Slider.min 1
        , Slider.step 1
        ]
      )
      ( text (formatInt model.userTxPerWeek))
    , hr [] []
    , dataTable
      "Average transaction size"
      "The average Bitcoin transaction size."
      (text "")
      (text (formatBytes model.avgTxSize))
    ]

blockchainView : String -> Model -> List (Html Msg)
blockchainView title model =
    [ h4 [] [ text title ]
    , dataTable
        "Block size"
        "Do not edit this setting if you only have a 64kbit modem ;-)"
        (Slider.view
            [ Slider.onChange BlocksizeChange
            , Slider.value (toFloat model.blocksize)
            , Slider.max 128000000
            , Slider.min 1000000
            , Slider.step 1000000
            ]
        )
        (text (formatBytes model.blocksize))
    ]

main : Program Never Model Msg
main =
    Html.program
        { init = ( model, Cmd.none )
        , view = view
        , subscriptions = always Sub.none
        , update = update
        }
