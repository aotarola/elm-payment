module Payment
  ( validateCardNumber
  ) where

import Payment.Internal exposing (..)

validateCardNumber : String -> Bool
validateCardNumber num =
  let
    numNoWhitespaces = stripWhitespaces num
  in
    case (cardFromNumber numNoWhitespaces) of
      Just card ->
        not card.luhn || luhnCheck numNoWhitespaces
      Nothing ->
        False
