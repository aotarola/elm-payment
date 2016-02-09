module Payment
  ( validateCardNumber
  ) where

import Payment.Internal exposing (..)

validateCardNumber : String -> Bool
validateCardNumber num =
  let
    num' = stripWhitespaces num
  in
    case (cardFromNumber num') of
      Just card ->
        not card.luhn || luhnCheck num'
      Nothing ->
        False
