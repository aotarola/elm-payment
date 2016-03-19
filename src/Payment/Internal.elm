module Payment.Internal (..) where

import Payment.Types exposing (CardType(..))
import Regex
import String


type alias CardSpec =
  { cardType : CardType
  , patterns : List Int
  , length : List Int
  , cvcLength : List Int
  , format : Regex.Regex
  , luhnCheck : Bool
  }


cardsSpec : List CardSpec
cardsSpec =
  [ --DEBIT CARDS
    { cardType = VISAELECTRON
    , patterns =
        [ 4026
        , 417500
        , 4405
        , 4508
        , 4844
        , 4913
        , 4917
        ]
    , length = [ 16 ]
    , cvcLength = [ 3 ]
    , format = defaultFormat
    , luhnCheck = True
    }
  , { cardType = MAESTRO
    , patterns =
        [ 5018
        , 502
        , 503
        , 56
        , 58
        , 639
        , 6220
        , 67
        ]
    , length = [12..19]
    , cvcLength = [ 3 ]
    , format = defaultFormat
    , luhnCheck = True
    }
  , { cardType = FORBRUGSFORENINGEN
    , patterns = [ 600 ]
    , format = defaultFormat
    , length = [ 16 ]
    , cvcLength = [ 3 ]
    , luhnCheck = True
    }
  , { cardType = DANKORT
    , patterns = [ 5019 ]
    , format = defaultFormat
    , length = [ 16 ]
    , cvcLength = [ 3 ]
    , luhnCheck = True
    }
  , -- CREDIT CARDS
    { cardType = VISA
    , patterns = [ 4 ]
    , length = [ 16 ]
    , cvcLength = [ 3 ]
    , format = defaultFormat
    , luhnCheck = True
    }
  , { cardType = MASTERCARD
    , patterns =
        [ 51
        , 52
        , 53
        , 54
        , 55
        , 22
        , 23
        , 24
        , 25
        , 26
        , 27
        ]
    , format = defaultFormat
    , length = [ 16 ]
    , cvcLength = [ 3 ]
    , luhnCheck = True
    }
  , { cardType = AMEX
    , patterns = [ 34, 37 ]
    , format = Regex.regex "(\\d{1,4})(\\d{1,6})?(\\d{1,5})?"
    , length = [ 15 ]
    , cvcLength = [3..4]
    , luhnCheck = True
    }
  , { cardType = DINERSCLUB
    , patterns =
        [ 30
        , 36
        , 38
        , 39
        ]
    , format = Regex.regex "(\\d{1,4})(\\d{1,6})?(\\d{1,4})?"
    , length = [ 14 ]
    , cvcLength = [ 3 ]
    , luhnCheck = True
    }
  , { cardType = DISCOVER
    , patterns =
        [ 60
        , 64
        , 65
        , 622
        ]
    , format = defaultFormat
    , length = [ 16 ]
    , cvcLength = [ 3 ]
    , luhnCheck = True
    }
  , { cardType = UNIONPAY
    , patterns = [ 62, 88 ]
    , format = defaultFormat
    , length = [16..19]
    , cvcLength = [ 3 ]
    , luhnCheck = False
    }
  , { cardType = JCB
    , patterns = [ 35 ]
    , format = defaultFormat
    , length = [ 16 ]
    , cvcLength = [ 3 ]
    , luhnCheck = True
    }
  ]


defaultFormat : Regex.Regex
defaultFormat =
  Regex.regex "(\\d{1,4})(\\d{1,4})?(\\d{1,4})?(\\d{1,4})?"


stripWhitespaces : String -> String
stripWhitespaces =
  Regex.replace Regex.All (Regex.regex "\\s+") (\_ -> "")


find : (a -> Bool) -> List a -> Maybe a
find fn list =
  case list of
    [] ->
      Nothing

    first :: rest ->
      if fn first then
        Just first
      else
        find fn rest


cardSpecFromCardType : CardType -> Maybe CardSpec
cardSpecFromCardType cardType =
  find (\card -> card.cardType == cardType) cardsSpec


grouped : Int -> List a -> List (List a)
grouped k xs =
  let
    len =
      List.length xs
  in
    if len > k then
      List.take k xs :: grouped k (List.drop k xs)
    else
      [ xs ]


matchPattern : String -> Int -> Bool
matchPattern num pattern =
  let
    stringPattern =
      (toString pattern)

    slicedNum =
      num
        |> String.slice 0 (String.length stringPattern)
  in
    slicedNum == stringPattern


matchCardSpec : String -> CardSpec -> Bool
matchCardSpec num cardSpec =
  case find (matchPattern num) cardSpec.patterns of
    Just pattern ->
      True

    Nothing ->
      False


cardSpecFromNumber : String -> Maybe CardSpec
cardSpecFromNumber num =
  find (matchCardSpec num) cardsSpec


luhnCheck : String -> Bool
luhnCheck num =
  let
    doubler pair =
      Maybe.withDefault 0 (List.head pair)
        + (if List.length pair > 1 then
            pair
              |> List.reverse
              |> List.head
              |> Maybe.withDefault 0
              |> (\x ->
                    x
                      * 2
                      - (if x >= 5 then
                          9
                         else
                          0
                        )
                 )
           else
            0
          )
  in
    num
      |> String.reverse
      |> String.split ""
      |> List.map (\a -> Result.withDefault 0 (String.toInt a))
      |> grouped 2
      |> List.foldl (\b acc -> acc + doubler b) 0
      |> (\x ->
            if x % 10 == 0 then
              True
            else
              False
         )
