module Tests where

import Payment exposing (validateCardNumber, validateCardCVC)
import ElmTest exposing (..)


validateCardNumberSuite : Test
validateCardNumberSuite =
  suite
    "validateCardNumber example tests"
    [ test "Visa" (assert (validateCardNumber 4485383939331480))
    , test "American Express" (assert (validateCardNumber 344235991129856))
    , test "Diners Club" (assert (validateCardNumber 30204415359894))
    , test "Discover" (assert (validateCardNumber 6011539859023682))
    , test "Master Card" (assert (validateCardNumber 5198181454082826))
    ]

validateCardCVCSuite : Test
validateCardCVCSuite =
  suite
    "validateCardCVC example tests"
    [ test "Any" (assert (validateCardCVC 537 Nothing))
    , test "Any" (assert (validateCardCVC 5376 Nothing))
    , test "Any" (assertEqual False (validateCardCVC 53 Nothing))
    , test "Any" (assertEqual False (validateCardCVC 53413 Nothing))
    , test "Visa" (assert (validateCardCVC 537 (Just "visa")))
    , test "Discover" (assert (validateCardCVC 123 (Just "discover")))
    ]


all : Test
all =
  suite
    "Payment test suite"
    [ validateCardNumberSuite
    , validateCardCVCSuite
    ]
