module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class)
import Material
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Locale)
import Material.Slider as Slider
import Material.Toggles as Toggles
import Material.Options as Options
import Material.Grid exposing (grid, cell, size, Device(..))


-- MODEL

type alias Model =
    { blocksize : Int
    , avgTxSize : Int
    , userTxPerWeek : Int
    , segwitActive : Bool
    , mdl :
        Material.Model
        -- Boilerplate: model store for any and all Mdl components you use.
    }


model : Model
model =
    { blocksize = 1000000
    , avgTxSize = 500
    , userTxPerWeek = 7
    , segwitActive = False
    , mdl =
        Material.model
        -- Boilerplate: Always use this initial Mdl model store.
    }

-- ACTION, UPDATE


type Msg
    = BlocksizeChange Float
    | UserTxPerWeekChange Float
    | SegWitActiveChange
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

        SegWitActiveChange ->
            ( { model | segwitActive = not model.segwitActive }
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

formatFloat : Float -> String
formatFloat float = format { myLocale | decimals = 2 } float

formatBytes : Int -> String
formatBytes int =
    if int >= 10^12 then format myLocale (toFloat int / toFloat 10^12) ++ " TB"
    else if int >= 10^9 then format myLocale (toFloat int / toFloat 10^9) ++ " GB"
    else if int >= 10^6 then format myLocale (toFloat int / toFloat 10^6) ++ " MB"
    else if int >= 10^3 then format myLocale (toFloat int / toFloat 10^3) ++ " KB"
    else toString int ++ " B"

effectiveBlocksize : Model -> Int
effectiveBlocksize model =
    if model.segwitActive then
        round ((toFloat model.blocksize) * 2.1)
    else
        model.blocksize

txPerBlock : Model -> Int
txPerBlock model = round (toFloat (effectiveBlocksize model) / toFloat model.avgTxSize)

blocksPerWeek : Int
blocksPerWeek = round (7 * 24 * 60 / 10.0)

txPerWeek : Model -> Int
txPerWeek model = blocksPerWeek * txPerBlock model

txPerSecond : Model -> Float
txPerSecond model = toFloat (txPerBlock model) / (10 * 60)

usersSupported : Model -> Int
usersSupported model = round (toFloat (txPerWeek model) / toFloat (model.userTxPerWeek))

maxGrowthPerYear : Model -> Int
maxGrowthPerYear model = blocksPerWeek * 52 * (effectiveBlocksize model)

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
      [ cell [ size All 4 ] (blockchainView "Blockchain" model)
      , cell [ size All 4 ] (usageView "Usage" model)
      , cell [ size All 4 ] (resultView "Result" model)
      ]

resultView : String -> Model -> List (Html Msg)
resultView title model =
    [ h4 [] [ text title ]
    , dataTable
      "Users supported"
      "How many users can use Bitcoin with these settings"
      ( text "")
      ( text (formatInt (usersSupported model) ++ " users"))
    , hr [] []
    , dataTable
      "Blockchain growth"
      "The maximum Blockchain growth per year"
      ( text "")
      ( text (formatBytes (maxGrowthPerYear model) ++ " / year"))
    , hr [] []
    , dataTable
      "Throughput"
      "Transactions per second possible without creating a growing backlog. 2000 TX/s is usually called 'Visa level'."
      ( text "")
      ( text (formatFloat (txPerSecond model) ++ " TX / second"))
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
      ( text ((formatInt model.userTxPerWeek) ++ " TX / week"))
    , hr [] []
    , dataTable
      "Transaction size"
      "The average Bitcoin transaction size in bytes"
      (text "")
      (text (formatBytes model.avgTxSize))
    ]

blockchainView : String -> Model -> List (Html Msg)
blockchainView title model =
    [ h4 [] [ text title ]
    , dataTable
        "Block size"
        "The base block size limit"
        (Slider.view
            [ Slider.onChange BlocksizeChange
            , Slider.value (toFloat model.blocksize)
            , Slider.max 128000000
            , Slider.min 1000000
            , Slider.step 1000000
            ]
        )
        (text (formatBytes model.blocksize))
      , hr [] []
      , dataTable
          "Segregated Witness"
          "Adds the blocksize-extending effect of Segregated Witness"
          ( Toggles.switch Mdl [0] model.mdl
              [ Options.onToggle SegWitActiveChange
              , Toggles.ripple
              , Toggles.value model.segwitActive
              ]
              [ text "" ]
          )
          (text "")
    ]

main : Program Never Model Msg
main =
    Html.program
        { init = ( model, Cmd.none )
        , view = view
        , subscriptions = always Sub.none
        , update = update
        }
