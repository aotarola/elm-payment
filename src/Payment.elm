module Payment (..) where

import Payment.Internal exposing (..)
import String
import Maybe exposing (andThen)
import Date exposing (Date)
import Date.Compare as DateCompare exposing (Compare2(..))
import Date.Core as DateCore
import Regex exposing (HowMany(All))


formatCardNumber : String -> Maybe String
formatCardNumber num =
  let
    format card =
      case List.head (Regex.find All card.format num) of
        Just match ->
          match.submatches
            |> List.map (\submatch -> Maybe.withDefault "" submatch)
            |> String.join " "
            |> String.trim
            |> Just

        Nothing ->
          Nothing
  in
    cardFromNumber num
      |> (flip andThen) format


validateCardNumber : String -> Bool
validateCardNumber num =
  case cardFromNumber num of
    Just card ->
      not card.luhn || luhnCheck num

    Nothing ->
      False


validateCardCVC : String -> Maybe String -> Bool
validateCardCVC cvc cardType =
  let
    cvcLength =
      String.length cvc
  in
    case cardType `andThen` cardFromCardType of
      Just card ->
        List.member cvcLength card.cvcLength

      Nothing ->
        cvcLength >= 3 && cvcLength <= 4


validateCardExpiry : Date -> Date -> Bool
validateCardExpiry expires date =
  let
    expires' =
      DateCore.firstOfNextMonthDate expires
  in
    DateCompare.is After expires' date
