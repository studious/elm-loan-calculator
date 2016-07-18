port module Loan exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onBlur, onClick, targetValue)
import MonthlyObligation
import Debug exposing (log)
import String exposing (toFloat, toInt)
import Json.Decode as Json

main =
    Html.beginnerProgram { model = model, view = view, update = update }

-- MODEL

type alias Model =
    { principal : Float
    , years : Float
    , rate : Float
    , monthly : MonthlyObligation.Model
    }

defaultModel =
    { principal = 200000
    , years = 30
    , rate = 5.0
    , monthly = MonthlyObligation.init
    }

model = defaultModel

-- UDPATE

type Msg
    = Calculate
    | Principal String
    | Years String
    | Rate String

update : Msg -> Model -> Model
update msg model =
    case msg of
        Calculate msg ->
            { model | monthly = MonthlyObligation.update (MonthlyObligation.Calculate model.principal model.years model.rate) model.monthly }
        Principal principal ->
            { model | principal = Result.withDefault defaultModel.principal (String.toFloat principal) }
        Years years ->
            { model | years = Result.withDefault defaultModel.years (String.toFloat years) }
        Rate rate ->
            { model | rate = Result.withDefault defaultModel.rate (String.toFloat rate) }

onMyBlur : (String -> msg) -> Attribute msg
onMyBlur tagger =
    Html.Events.on "blur" (Json.map tagger targetValue)

-- VIEW

view : Model -> Html Msg
view model =
    div 
        []
        [ pageHeader
        , div
            [ class "content" ]
            [ div
                [ class "form" ]
                [ div 
                    []
                    [ label [] [ text "Principal:" ]
                    , input [
                        type' "text"
                        , value (toString model.principal) 
                        , onMyBlur Principal ] []
                    ]
                , div
                    []
                    [ label [] [ text "Years:" ]
                    , input [
                        type' "text"
                        , value (toString model.years)
                        , onMyBlur Years ] []
                    ]
                , div
                    []
                    [ label [] [ text "Rate:" ]
                    , input [
                        type' "text"
                        , value (toString model.rate)
                        , onMyBlur Rate ] []
                    ]
                , div
                    []
                    [ label [] [ ]
                    , button [
                        onClick Calculate ]
                        [ text "Calculate" ]
                    ]
                ]
            , Html.map Calculate (MonthlyObligation.view model.monthly)
            ]
        ]

pageHeader : Html msg
pageHeader =
    header
        []
        [ h1
            []
            [ text "Mortgage Calculator" ]
        ]

