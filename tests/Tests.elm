module Tests (..) where

import Payment exposing (..)
import Payment.Types exposing (Card, CardType(..))
import ElmTest exposing (..)
import Date.Utils as DateUtils


emptyCard : Card
emptyCard =
  { number = ""
  , cvc = ""
  , expires = ""
  , cardType = Nothing
  }


validateCardNumberSuite : Test
validateCardNumberSuite =
  let
    validVisa =
      { emptyCard | number = "4485383939331480" }

    validVisaWS =
      { emptyCard | number = "4485383939331480" }

    invalidVisa =
      { emptyCard | number = "4485x383939331480" }

    validAmex =
      { emptyCard | number = "344235991129856" }

    validDinersClub =
      { emptyCard | number = "30204415359894" }

    validDiscover =
      { emptyCard | number = "6011539859023682" }

    validMasterCard =
      { emptyCard | number = "5198181454082826" }
  in
    suite
      "validateCardNumber tests"
      [ test "Visa" (assert (validateCardNumber validVisa))
      , test "Visa (with whitespaces)" (assert (validateCardNumber validVisaWS))
      , test "Visa (with a character)" (assertEqual False (validateCardNumber invalidVisa))
      , test "American Express" (assert (validateCardNumber validAmex))
      , test "Diners Club" (assert (validateCardNumber validDinersClub))
      , test "Discover" (assert (validateCardNumber validDiscover))
      , test "Master Card" (assert (validateCardNumber validMasterCard))
      , test "Empty string" (assertEqual False (validateCardNumber emptyCard))
      ]


validateCardCVCSuite : Test
validateCardCVCSuite =
  let
    validCard1 =
      { emptyCard | cvc = "537" }

    validCard2 =
      { emptyCard | cvc = "5376" }

    invalidCard1 =
      { emptyCard | cvc = "53" }

    invalidCard2 =
      { emptyCard | cvc = "53413" }

    validCardWS =
      { emptyCard | cvc = " 5 3 7" }

    invalidCard =
      { emptyCard | cvc = "abc" }

    validCardWithType1 =
      { emptyCard
        | cvc = "537"
        , cardType = Just VISA
      }

    validCardWithType2 =
      { emptyCard
        | cvc = "123"
        , cardType = Just DISCOVER
      }
  in
    suite
      "validateCardCVC tests"
      [ test "Any" (assert (validateCardCVC validCard1))
      , test "Any (with whitespaces)" (assert (validateCardCVC validCardWS))
      , test "Any (with characters)" (assertEqual False (validateCardCVC invalidCard))
      , test "Any" (assert (validateCardCVC validCard2))
      , test "Any" (assertEqual False (validateCardCVC invalidCard1))
      , test "Any" (assertEqual False (validateCardCVC invalidCard2))
      , test "Visa" (assert (validateCardCVC validCardWithType1))
      , test "Discover" (assert (validateCardCVC validCardWithType2))
      , test "Empty String" (assertEqual False (validateCardCVC emptyCard))
      ]


validateCardExpirySuite : Test
validateCardExpirySuite =
  let
    -- February 13, 2016
    currentDate =
      DateUtils.unsafeFromString "Aug 1, 2015"

    validCard =
      { emptyCard | expires = "3 / 2016" }

    invalidCard =
      { emptyCard | expires = "7 / 2015" }
  in
    suite
      "validateCardExpiry tests"
      [ test "Should pass for 3 / 2016" (assert (validateCardExpiry validCard currentDate))
      , test "Should fail for 7 / 2015" (assertEqual False (validateCardExpiry invalidCard currentDate))
      ]


formatCardNumberSuite : Test
formatCardNumberSuite =
  let
    fullVisaCard =
      { emptyCard | number = "4111111111111111" }

    partialVisaCard =
      { emptyCard | number = "41111" }

    fullAmexCard =
      { emptyCard | number = "3442 35991129856" }

    partialAmexCard =
      { emptyCard | number = "34423599112" }

    fullDinersClubCard =
      { emptyCard | number = "30204415359894" }

    partialDinersClubCard =
      { emptyCard | number = "302044153598" }

    invalidCard1 =
      { emptyCard | number = "9999999999999999999" }

    invalidCard2 =
      { emptyCard | number = "1233a9" }
  in
    suite
      "formatCardNumber tests"
      [ test "Full Visa number" (assertEqual "4111 1111 1111 1111" (formatCardNumber fullVisaCard).number)
      , test "Partial Visa number" (assertEqual "4111 1" (formatCardNumber partialVisaCard).number)
      , test "Full American Express number" (assertEqual "3442 359911 29856" (formatCardNumber fullAmexCard).number)
      , test "Partial American Express number" (assertEqual "3442 359911 2" (formatCardNumber partialAmexCard).number)
      , test "Full Diners Club number" (assertEqual "3020 441535 9894" (formatCardNumber fullDinersClubCard).number)
      , test "Partial Diners Club number" (assertEqual "3020 441535 98" (formatCardNumber partialDinersClubCard).number)
      , test "Bad credit card (long number)" (assertEqual "9999999999999999999" (formatCardNumber invalidCard1).number)
      , test "Bad credit card (numbers and letters)" (assertEqual "1233a9" (formatCardNumber invalidCard2).number)
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
