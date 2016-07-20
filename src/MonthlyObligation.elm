module MonthlyObligation exposing (Model, Msg(..), init, view, update)
import Html exposing (..)
import Exts.Float as Round
import Html.Attributes exposing (..)

-- MODEL

type alias Model =
    { monthlyPayment : Maybe Float
    , monthlyRate : Maybe Float
    }

init : Model
init = 
    { monthlyPayment = Nothing
    , monthlyRate = Nothing
    }

calculateMonthlyPayment : Float -> Float -> Float -> Model
calculateMonthlyPayment principal years rate =
    let
        monthlyRate =
            rate / 100 / 12

        monthlyPayment =
            principal * monthlyRate / (1 - (1/(1 + monthlyRate)^(years * 12)))
    in
        { monthlyPayment = Just monthlyPayment, monthlyRate = Just monthlyRate }

type Msg
    = Calculate Float Float Float

update : Msg -> Model -> Model
update msg model =
    case msg of
        Calculate principal years rate ->
            calculateMonthlyPayment principal years rate


view : Model -> Html Msg
view model =
    div [] [
        h2
            []
            [ text "Monthly Payment: "
            , span [ id "monthlyPayment", class "currency" ] 
                [
                    case model.monthlyPayment of
                        Nothing -> text ""
                        Just monthlyPayment -> text (toString <| Round.roundTo 2 monthlyPayment)
                ]
            ]
        , h3
            []
            [ text "Monthly Rate: "
            , span [ id "monthlyRate" ] 
                [
                    case model.monthlyRate of
                        Nothing -> text ""
                        Just monthlyRate -> text (toString <| Round.roundTo 4 monthlyRate)
                ]
            ]
        ]
