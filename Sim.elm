module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href, target)
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
    , segwitFactor : Float
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
    , segwitFactor = 1.6
    , mdl =
        Material.model
        -- Boilerplate: Always use this initial Mdl model store.
    }

-- ACTION, UPDATE


type Msg
    = BlocksizeChange Float
    | UserTxPerWeekChange Float
    | SegWitFactorChange Float
    | TxSizeChange Float
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

        TxSizeChange value ->
            ( { model | avgTxSize = round value }
            , Cmd.none
            )

        SegWitActiveChange ->
            ( { model | segwitActive = not model.segwitActive }
            , Cmd.none
            )

        SegWitFactorChange value ->
            ( { model | segwitFactor = value }
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

formatFloat : Int -> Float -> String
formatFloat ds float = format { myLocale | decimals = ds } float

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
        round ((toFloat model.blocksize) * model.segwitFactor)
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

dataTable : String -> List (Html Msg) -> Html Msg -> Html Msg -> Html Msg
dataTable title caption e1 e2 =
  div []
      [ div [] [ text title ]
      , div [ class "caption" ] caption
      , table []
          [ tr []
              [ td [] [ e1 ]
              , td [] [ e2 ]
              ]
          ]
      ]

dataTable3 : String -> List (Html Msg) -> Html Msg -> Html Msg -> Html Msg -> Html Msg
dataTable3 title caption e1 e2 e3 =
  div []
      [ div [] [ text title ]
      , div [ class "caption" ] caption
      , table []
          [ tr []
              [ td [] [ e1 ]
              , td [] [ e2 ]
              , td [] [ e3 ]
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
      [ text "How many users can use Bitcoin with these settings" ]
      ( text "")
      ( text (formatInt (usersSupported model) ++ " users"))
    , hr [] []
    , dataTable
      "Blockchain growth"
      [ text "The maximum Blockchain growth per year" ]
      ( text "")
      ( text (formatBytes (maxGrowthPerYear model) ++ " / year"))
    , hr [] []
    , dataTable
      "Throughput"
      [ text "Transactions per second possible without creating a growing backlog. 2000 TX/s is usually called 'Visa level'." ]
      ( text "")
      ( text (formatFloat 2 (txPerSecond model) ++ " TX / second"))
    ]

usageView : String -> Model -> List (Html Msg)
usageView title model =
    [ h4 [] [ text title ]
    , dataTable
      "User transactions per week"
      [ text "How many transaction the average Bitcoin user does per week" ]
      ( Slider.view
        [ Slider.onChange UserTxPerWeekChange
        , Slider.value (toFloat model.userTxPerWeek)
        , Slider.max 100
        , Slider.min 1
        , Slider.step 1
        ]
      )
      ( text ((formatInt model.userTxPerWeek) ++ " TX / week"))
    , hr [] []
    , dataTable
      "Transaction size"
      [ text "The average Bitcoin transaction size in bytes. "
      , a [ href "https://tradeblock.com/bitcoin/historical/1h-f-tsize_per_avg-01101", target "_blank" ] [ text "Current statistics" ]
      ]
      ( Slider.view
        [ Slider.onChange TxSizeChange
        , Slider.value (toFloat model.avgTxSize)
        , Slider.max 1000
        , Slider.min 100
        , Slider.step 50
        ]
      )
      (text (formatBytes model.avgTxSize))
    ]

blockchainView : String -> Model -> List (Html Msg)
blockchainView title model =
    [ h4 [] [ text title ]
    , dataTable
        "Block size"
        [ text "The base block size limit" ]
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
      , dataTable3
          "Segregated Witness"
          [ text "Adds a blocksize-extending effect. "
          , a [ href "https://lists.linuxfoundation.org/pipermail/bitcoin-dev/2015-December/011869.html", target "_blank" ] [ text "Estimated factor" ]
          ]
          ( Toggles.switch Mdl [0] model.mdl
              [ Options.onToggle SegWitActiveChange
              , Toggles.ripple
              , Toggles.value model.segwitActive
              ]
              [ text "" ]
          )
          (Slider.view
              [ Slider.onChange SegWitFactorChange
              , Slider.value model.segwitFactor
              , Slider.max 4
              , Slider.min 1
              , Slider.step 0.1
              ]
          )
          (text (formatFloat 1 model.segwitFactor))
    ]

main : Program Never Model Msg
main =
    Html.program
        { init = ( model, Cmd.none )
        , view = view
        , subscriptions = always Sub.none
        , update = update
        }
