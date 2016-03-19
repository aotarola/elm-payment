module Payment (..) where

import Payment.Internal exposing (..)
import Payment.Types exposing (Card)
import String
import Maybe exposing (andThen)
import Date exposing (Date)
import Date.Compare as DateCompare exposing (Compare2(..))
import Date.Core as DateCore
import Regex exposing (HowMany(All))


formatCardExpiry : Card -> Card
formatCardExpiry card =
  let
    expiryFormat =
      Regex.regex "(\\d{1,2})[^\\d]*(\\d{1,4})?"

    format submatches =
      String.join " / " (List.map (\x -> Maybe.withDefault "" x) submatches)
  in
    case List.head (Regex.find All expiryFormat card.expires) of
      Just match ->
        { card
          | expires =
              format match.submatches
        }

      Nothing ->
        card


formatCardNumber : Card -> Card
formatCardNumber card =
  let
    num =
      stripWhitespaces card.number

    cardSpec =
      cardSpecFromNumber num

    format cardSpec =
      case List.head (Regex.find All cardSpec.format num) of
        Just match ->
          match.submatches
            |> List.map (\submatch -> Maybe.withDefault "" submatch)
            |> String.join " "
            |> String.trim

        Nothing ->
          num
  in
    case cardSpec of
      Just cardSpec ->
        { card
          | number = format cardSpec
          , cardType = Just cardSpec.cardType
        }

      Nothing ->
        card


validateCardNumber : Card -> Bool
validateCardNumber card =
  let
    num =
      stripWhitespaces card.number
  in
    case cardSpecFromNumber num of
      Just card ->
        not card.luhnCheck || luhnCheck num

      Nothing ->
        False


validateCardCVC : Card -> Bool
validateCardCVC card =
  let
    cvc =
      stripWhitespaces card.cvc

    cardType =
      card.cardType

    cvcLength =
      String.length cvc
  in
    case String.toInt cvc of
      Ok _ ->
        case (flip andThen) cardSpecFromCardType cardType of
          Just card ->
            List.member cvcLength card.cvcLength

          Nothing ->
            cvcLength >= 3 && cvcLength <= 4

      Err _ ->
        False


validateCardExpiry : Card -> Date -> Bool
validateCardExpiry card currentDate =
  let
    expiresStr =
      case String.split "/" card.expires of
        month :: year ->
          month ++ "/01/" ++ Maybe.withDefault "" (List.head year)

        [] ->
          ""
  in
    case Date.fromString expiresStr of
      Ok expires ->
        DateCompare.is After (DateCore.firstOfNextMonthDate expires) currentDate

      Err _ ->
        False
