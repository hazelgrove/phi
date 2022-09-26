{
module Lexer where

import Algo
}

%wrapper "basic"

tokens :-

"type" {\_ -> TKTypLet}
"let" {\_ -> TKExpLet}
"=" {\_ -> TKEq}
"in" {\_ -> TKIn}
":" {\_ -> TKTypAsc}
"Bse" {\_ -> TKBse}
"\" {\_ -> TKλ}
"λ" {\_ -> TKλ}
"*" {\_ -> TKBinop}
"⊕" {\_ -> TKBinop}
"::" {\_ -> TKKndAsc}
$white+ {\_ -> TKAp}
"." {\_ -> TKDot}
"(" {\_ -> TKLP}
")" {\_ -> TKRP}
"Type" {\_ -> TKType}
"KHole" {\_ -> TKKHole}
"S" {\_ -> TKS}
"P" {\_ -> TKΠ}
"Π" {\_ -> TKΠ}
[a-zA-Z]+_?[0-9]* {\s -> TKVar s}

{
data Token
  = TKTypLet
  | TKExpLet
  | TKEq
  | TKIn
  | TKTypAsc
  | TKVar TID
  | TKBse
  | TKBinop
  | TKλ
  | TKKndAsc
  | TKAp
  | TKDot
  | TKLP
  | TKRP
  | TKType
  | TKKHole
  | TKS
  | TKΠ
  deriving (Show, Eq)
}
