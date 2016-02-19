module Tests where

import Payment exposing ( validateCardNumber
                        , validateCardCVC
                        , validateCardExpiry
                        , formatCardNumber
                        )
import ElmTest exposing (..)
import Date

validateCardNumberSuite : Test
validateCardNumberSuite =
  suite
    "validateCardNumber tests"
    [ test "Visa" (assert (validateCardNumber 4485383939331480))
    , test "American Express" (assert (validateCardNumber 344235991129856))
    , test "Diners Club" (assert (validateCardNumber 30204415359894))
    , test "Discover" (assert (validateCardNumber 6011539859023682))
    , test "Master Card" (assert (validateCardNumber 5198181454082826))
    ]

validateCardCVCSuite : Test
validateCardCVCSuite =
  suite
    "validateCardCVC tests"
    [ test "Any" (assert (validateCardCVC 537 Nothing))
    , test "Any" (assert (validateCardCVC 5376 Nothing))
    , test "Any" (assertEqual False (validateCardCVC 53 Nothing))
    , test "Any" (assertEqual False (validateCardCVC 53413 Nothing))
    , test "Visa" (assert (validateCardCVC 537 (Just "visa")))
    , test "Discover" (assert (validateCardCVC 123 (Just "discover")))
    ]

validateCardExpirySuite : Test
validateCardExpirySuite =
  let
    -- February 13, 2016
    currentDate = Date.fromTime 1455416393417.0
    -- March 10, 2016
    validDate = Date.fromTime 1457568000000.0
    -- February 1, 2016
    expiredDate = Date.fromTime 1454284800000.0
  in
    suite
      "validateCardExpiry tests"
      [ test "Should pass for 3 / 2016" (assert (validateCardExpiry validDate currentDate))
      , test "Should fail for 1 / 2016" (assertEqual False (validateCardExpiry expiredDate currentDate))
      ]

formatCardNumberSuite : Test
formatCardNumberSuite =
  suite
    "formatCardNumber tests"
    [ test "Complete Visa number" (assertEqual "4111 1111 1111 1111" (formatCardNumber 4111111111111111))
    , test "Partial Visa number" (assertEqual "4111 1" (formatCardNumber 41111))
    , test "Complete American Express number" (assertEqual "3442 359911 29856" (formatCardNumber 344235991129856))
    , test "Partial American Express number" (assertEqual "3442 359911 2" (formatCardNumber 34423599112))
    , test "Complete Diners Club number" (assertEqual "3020 441535 9894" (formatCardNumber 30204415359894))
    , test "Partial Diners Club number" (assertEqual "3020 441535 98" (formatCardNumber 302044153598))
    ]

all : Test
all =
  suite
    "Payment test suite"
    [ validateCardNumberSuite
    , validateCardCVCSuite
    , validateCardExpirySuite
    , formatCardNumberSuite
    ]
