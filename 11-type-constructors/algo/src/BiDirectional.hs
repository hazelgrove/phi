{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module BiDirectional where

import Common
import Data.Maybe
import qualified ECtx
import qualified External as E
import qualified ICtx
import qualified Internal as I
import Normalize hiding ((⊳→))

syn :: ECtx.Ctx -> E.Exp -> Maybe I.Term
syn eΓ eδ = do
  iΓ <- fixCtx eΓ
  syn' iΓ eδ

syn' :: ICtx.Ctx -> E.Exp -> Maybe I.Term
syn' iΓ (E.EVar t) = ICtx.lookupE iΓ t
syn' iΓ (E.Eλ x eτ eδ) = do
  SER {term = iδ1, ..} <- syn_elab' iΓ eτ
  iδ2 <- syn' (iΓ ICtx.⌢⌢⌢ (x, iδ1)) eδ
  return $ iδ1 I.:⊕ iδ2
syn' iΓ (E.EAp eδ1 eδ2) = do
  iδ1 <- syn' iΓ eδ1
  let MATR {..} = iΓ |- (iδ1 ⊳→)
  ana iΓ eδ2 iδIn &>> return iδOut
syn' iΓ (E.ETypLet t eτ eδ) = do
  SER {typ = iτ, term = iδ, ..} <- syn_elab' iΓ eτ
  let innerΓ = (iΓ ICtx.⌢ (t, iτ))
  iδ' <- syn' innerΓ eδ
  return $ insensitiveβnormal (subst iδ t iδ')
syn' iΓ (E.EExpLet x eτ eδDef eδBody) = do
  SER {term = iδ, ..} <- syn_elab' iΓ eτ
  ana iΓ eδDef iδ &>> syn' (iΓ ICtx.⌢⌢⌢ (x, iδ)) eδBody

ana :: ICtx.Ctx -> E.Exp -> I.Term -> Bool
ana iΓ eδ iδ' = fromMaybe False (syn' iΓ eδ >>= \iδ -> Just (iΓ |- (iδ ~ iδ' $ I.Type)))

data MATResult =
  MATR
    { iδIn :: I.Term
    , iδOut :: I.Term
    }
  deriving (Show)

-- TODO: type suggests this is to be generalized to ``consistent sub type (constructor)''
-- but right now is only ``consistent sub type (at kind Type)''
cst' :: ICtx.Ctx -> I.Term -> I.Term -> I.Typ -> Bool
cst' _ (I.ETHole _) _ _ = True
cst' _ _ (I.ETHole _) _ = True
cst' _ (I.NETHole _ _) _ _ = True
cst' _ _ (I.NETHole _ _) _ = True
cst' iΓ (iδ1 I.:⊕ iδ2) (iδ1' I.:⊕ iδ2') iτ =
  cst' iΓ iδ1 iδ1' iτ && cst' iΓ iδ2 iδ2' iτ
cst' iΓ iδ1 iδ2 iτ = tequiv' iΓ iδ1 iδ2 iτ

(~) :: I.Term -> I.Term -> I.Typ -> ICtx.Ctx -> Bool
(~) = ((.) . (.) $ flip) . ((.) flip) . flip $ cst'

mat' :: ICtx.Ctx -> I.Term -> MATResult
mat' _ (I.ETHole u) = MATR (I.ETHole u) (I.ETHole u)
mat' _ (iδ1 I.:⊕ iδ2) = MATR iδ1 iδ2
mat' aΓ δ =
  case wh_reduc aΓ δ of
    Left _ -> error $ "Can't (⊳→) on this bogus value: " ++ (show aΓ) ++ " ⊢ " ++ (show δ)
    Right δ' -> mat' aΓ δ'

(⊳→) = flip mat'
