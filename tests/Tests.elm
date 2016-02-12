module Tests where

import Payment exposing (validateCardNumber)
import ElmTest exposing (..)


paymentSuite : Test
paymentSuite =
  suite
    "validateCardNumber example tests"
    [ test "Visa" (assert (validateCardNumber 4485383939331480))
    , test "American Express" (assert (validateCardNumber 344235991129856))
    , test "Diners Club" (assert (validateCardNumber 30204415359894))
    , test "Discover" (assert (validateCardNumber 6011539859023682))
    , test "Master Card" (assert (validateCardNumber 5198181454082826))
    ]


all : Test
all =
  suite
    "Payment test suite"
    [ paymentSuite
    ]
