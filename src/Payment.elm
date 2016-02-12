module Payment
  ( validateCardNumber
  ) where

import Payment.Internal exposing (..)

validateCardNumber : Int -> Bool
validateCardNumber num =
  case (cardFromNumber num) of
    Just card ->
      not card.luhn || luhnCheck num
    Nothing ->
      False
