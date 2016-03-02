module Payment.Types (..) where


type alias Card =
  { number : String
  , cvc : String
  , expires : String
  , cardType : Maybe CardType
  }


type CardType
  = VISAELECTRON
  | MAESTRO
  | FORBRUGSFORENINGEN
  | DANKORT
  | VISA
  | MASTERCARD
  | AMEX
  | DINERSCLUB
  | DISCOVER
  | UNIONPAY
  | JCB
