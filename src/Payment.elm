module Payment
  ( validateCardNumber
  , validateCardCVC
  , validateCardExpiry
  ) where

import Payment.Internal exposing  (..)
import String
import Maybe exposing (andThen)
import Date exposing (Date)
import Date.Compare as DateCompare exposing (Compare2 (..))
import Date.Core as DateCore

validateCardNumber : Int -> Bool
validateCardNumber num =
  case (cardFromNumber num) of
    Just card ->
      not card.luhn || luhnCheck num
    Nothing ->
      False

validateCardCVC : Int -> Maybe String -> Bool
validateCardCVC cvc cardType =
  let
    cvcLength = cvc |> toString |> String.length
  in
    case cardType `andThen` cardFromCardType of
      Just card ->
        List.member cvcLength card.cvcLength
      Nothing ->
        cvcLength >= 3 && cvcLength <= 4

validateCardExpiry : Date -> Date -> Bool
validateCardExpiry expires date =
  let
    expires' = DateCore.firstOfNextMonthDate expires
  in
    DateCompare.is After expires' date
