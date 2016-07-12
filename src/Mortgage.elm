module Mortgage exposing (..)

{--
type alias Amortization =
    { principal : Float
    , interest : Float
    , balance : Float
    }
--}

-- MODEL

type alias MonthlyObligation =
    { monthlyPayment : Float
    , monthlyRate : Float
    }

-- UPDATE
{--
type Msg
    = Calculate (Float Float Float)

update : Msg -> Model -> Model
update msg model =
    case Calculate (Float Float Float) ->

-- VIEW

--view : Model -> Html Msg
--}




calculateMonthlyPayment : Float -> Float -> Float -> Maybe MonthlyObligation
calculateMonthlyPayment principal years rate =
    let
        monthlyRate =
            rate / 100 / 12

        monthlyPayment =
            principal * monthlyRate / (1 - (1/(1 + monthlyRate)^(years * 12)))
    in
        Just { monthlyPayment = monthlyPayment, monthlyRate = monthlyRate }
