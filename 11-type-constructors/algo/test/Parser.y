{
module Parser where

import Algo
import Lexer
}

%name parser
%tokentype{Token}
%error{parseError}

%token
var {TKVar $$}
bse {TKBse}
'⊕' {TKBinop}
'λ' {TKλ}
'::' {TKKndAsc}
' ' {TKAp}
'.' {TKDot}
'(' {TKLP}
')' {TKRP}
'Type' {TKType}
'KHole' {TKKHole}
'S' {TKS}
'Π' {TKΠ}

%%

Typ : var {TVar $1}
    | bse {Bse}
    | Typ '⊕' Typ {$1 :⊕ $3}
    | Typ ' ' '⊕' ' ' Typ {$1 :⊕ $5}
    | 'λ' var '::' Knd '.' Typ {Tλ $2 $4 $6}
    | 'λ' var '::' Knd '.' ' ' Typ {Tλ $2 $4 $7}
    | Typ ' ' Typ {TAp $1 $3}
    | '(' Typ ')' {$2}
Knd : 'Type' {Type}
    | 'KHole' {KHole}
    | 'S' Knd Typ {S $2 $3}
    | 'Π' var '::' Knd '.' Knd {Π $2 $4 $6}
    | '(' Knd ')' {$2}

{
parseError :: [Token] -> a
parseError _ = error "Parse error!\n"

parse :: String -> Typ
parse = parser . alexScanTokens
}
