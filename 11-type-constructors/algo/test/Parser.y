{
module Parser where

import Algo
import Lexer
}

%name expParser Exp
%name typParser Typ
%name kndParser Knd
%tokentype{Token}
%error{parseError}

%token
'type' {TKTypLet}
'let' {TKExpLet}
'=' {TKEq}
'in' {TKIn}
':' {TKTypAsc}
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

Exp : var {EVar $1}
    | 'λ' var ':' Typ '.' Exp {Eλ $2 $4 $6}
    | Exp ' ' Exp {EAp $1 $3}
    | 'type' ' ' var ' ' '=' ' ' Typ ' ' 'in' ' ' Exp {ETypLet $3 $7 $11}
    | 'let' ' ' var ' ' ':' ' ' Typ ' ' '=' ' ' Exp ' ' 'in' ' ' Exp {EExpLet $3 $7 $11 $15}
    | '(' Exp ')' {$2}
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

parseExp  :: String -> Exp
parseExp = expParser . alexScanTokens

parseTyp :: String -> Typ
parseTyp = typParser . alexScanTokens

parseKnd :: String -> Knd
parseKnd = kndParser . alexScanTokens
}
