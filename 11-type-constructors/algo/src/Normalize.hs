{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Normalize where

import Common
import qualified ECtx
import qualified External as E
import ICtx
import Internal

import Control.Exception.Base
import Control.Monad
import Data.Maybe
import Debug.Trace

-- NOTE: ' marks internal functions
-- non ' functions are ``public facing''
-- non ' functions perform more checks/conversions before calling ' version
-- internal functions typically call internal functions
fixKnd' :: Ctx -> E.Knd -> Maybe Typ
fixKnd' _ E.Type = return Type
fixKnd' _ E.KHole = return KHole
fixKnd' iΓ (E.S eκ eτ) = do
  iτ <- fixKnd' iΓ eκ
  AER {term = iδ} <- ana_elab' iΓ eτ iτ -- shouldn't have holes in initial ctx
  return $ S iτ iδ
fixKnd' iΓ (E.Π t eκ1 eκ2) = do
  iτ1 <- fixKnd' iΓ eκ1
  iτ2 <- fixKnd' (iΓ :⌢ (t, iτ1)) eκ2
  return $ Π t iτ1 iτ2

fixCtx :: ECtx.Ctx -> Maybe Ctx
fixCtx ECtx.Nil = Just Nil
fixCtx (eΓ ECtx.:⌢ (t, eκ)) = do
  iΓ <- fixCtx eΓ
  iτ <- fixKnd' iΓ eκ
  return $ iΓ ⌢ (t, iτ)

data SynElabResult =
  SER
    { typ :: Typ
    , term :: Term
    , iΓ :: Ctx -- named for shadowing
    }

syn_elab :: ECtx.Ctx -> E.Typ -> Maybe SynElabResult
syn_elab eΓ eτ = do
  iΓ <- fixCtx eΓ
  syn_elab' iΓ eτ

syn_elab' :: Ctx -> E.Typ -> Maybe SynElabResult
syn_elab' iΓ (E.TVar t)
  -- free variables should already be in a hole
 = do
  iτ <- lookupT iΓ t
  return $ SER (S iτ $ TVar t) (TVar t) iΓ
syn_elab' iΓ E.Bse = return $ SER (S Type Bse) Bse iΓ
syn_elab' iΓ (eτ1 E.:⊕ eτ2) = do
  AER {term = iδ1, ..} <- ana_elab' iΓ eτ1 Type
  AER {term = iδ2, ..} <- ana_elab' iΓ eτ2 Type
  return $ SER (S Type $ iδ1 :⊕ iδ2) (iδ1 :⊕ iδ2) iΓ
syn_elab' iΓ (E.ETHole u) =
  assert (isNothing $ lookupH iΓ u) $
  return $ SER (S KHole $ ETHole u) (ETHole u) (iΓ :⌢⌢ (u, KHole))
syn_elab' iΓ (E.NETHole u eτ) =
  assert (isNothing $ lookupH iΓ u) $ do
    SER {term = iδ, ..} <- syn_elab' iΓ eτ
    return $ SER (S KHole $ NETHole u iδ) (NETHole u iδ) (iΓ :⌢⌢ (u, KHole))
syn_elab' iΓ (E.Tλ t eκ eτ) = do
  iτ1 <- fixKnd' iΓ eκ
  SER {typ = iτ2, term = iδ, ..} <- syn_elab' (iΓ :⌢ (t, iτ1)) eτ
  return $ SER (S (Π t iτ1 iτ2) (Tλ t iτ1 iδ)) (Tλ t iτ1 iδ) iΓ
syn_elab' iΓ (E.TAp eτ1 eτ2) = do
  SER {typ = iτ1} <- syn_elab' iΓ eτ1 -- we don't have a plain syn
  MPKR {..} <- iΓ |- (iτ1 ⊳→)
  AER {term = iδ1, ..} <- ana_elab' iΓ eτ1 (Π tπ iτIn iτOut)
  AER {term = iδ2, ..} <- ana_elab' iΓ eτ2 (iτIn)
  return $ SER (subst iδ2 tπ iτOut) (TAp iδ1 iδ2) iΓ

data AnaElabResult =
  AER
    { term :: Term
    , iΓ :: Ctx -- named for shadowing
    }

-- NOTE: prob don't need ana_elab :: ECtx.Ctx
-- TODO: something something not holes
ana_elab' :: Ctx -> E.Typ -> Typ -> Maybe AnaElabResult
ana_elab' iΓ (E.ETHole u) iτ =
  assert (isNothing $ lookupH iΓ u) $ return $ AER (ETHole u) (iΓ :⌢⌢ (u, iτ))
ana_elab' iΓ (E.NETHole u eτ) iτ =
  assert (isNothing $ lookupH iΓ u) $ do
    SER {term = iδ, ..} <- syn_elab' iΓ eτ
    return $ AER (NETHole u iδ) (iΓ :⌢⌢ (u, iτ))
ana_elab' iΓ eτ iτ = do
  SER {typ = iτ', term = iδ, ..} <- syn_elab' iΓ eτ
  (iΓ |- (iτ' ≲ iτ)) &>> (return $ AER iδ iΓ)

-- TODO: We don't need this, right?
{-
τ_elab :: Ctx -> E.Knd -> Maybe Typ
τ_elab = undefined
-}
-- Maybe since ``otherwise''
wh_reduc :: Ctx -> Term -> Either Term Term
wh_reduc _ (TAp (Tλ t τ δ1) δ2) = assert (True) $ Right $ subst δ2 t δ1 {- check δ2 against τ -}
wh_reduc iΓ (TVar t) = undefined
-- wh_reduc iΓ Bse = Right $ path_normal iΓ Bse
wh_reduc iΓ δ@(TAp δ1 δ2) =
  case wh_reduc iΓ δ1 of
    Right δ1' -> assert (δ1' /= δ1) $ Right (TAp δ1' δ2)
    Left ωδ1 -> assert (ωδ1 == δ1) $ Left δ
wh_reduc _ δ = Left δ

wh_normal :: Ctx -> Term -> Term
wh_normal iΓ δ =
  case wh_reduc iΓ δ of
    Right δ' -> wh_normal iΓ δ'
    Left ωδ -> assert (ωδ == δ) $ δ

term_normal :: Ctx -> Term -> Typ -> Term
term_normal iΓ δ τ =
  let ωτ = type_normal iΓ τ
   in case ωτ of
        Type -> wh_normal iΓ δ

path_normal :: Ctx -> Term -> (Term, Typ)
path_normal _ Bse = (Bse, Type)

type_normal :: Ctx -> Typ -> Typ
type_normal _ Type = Type
type_normal _ KHole = KHole
type_normal iΓ (S τ δ) =
  let ωτ = type_normal iΓ τ
   in let ωδ = term_normal iΓ δ ωτ
       in case ωτ of
            Type -> S Type ωδ
            KHole -> S KHole ωδ
            S τ' δ' -> S τ' δ'
            Π t τ1 τ2 ->
              let t' = fresh t
               in let τ3 = αRename t' t τ2
                   in let ωδ' =
                            term_normal (iΓ :⌢ (t', τ1)) (TAp ωδ (TVar t')) τ3
                       in Π t' τ1 (S τ3 ωδ')

tequiv :: ECtx.Ctx -> E.Typ -> E.Typ -> E.Knd -> Bool
tequiv = undefined

kequiv' :: Ctx -> Typ -> Typ -> Bool
kequiv' = undefined

csk' :: Ctx -> Typ -> Typ -> Bool
csk' = undefined

(≲) = ((.) flip) . flip $ csk'

data MPKResult =
  MPKR
    -- for use with record wildcards
    { tπ :: TID
    , iτIn :: Typ
    , iτOut :: Typ
    }

mpk' :: Ctx -> Typ -> Maybe MPKResult
mpk' = undefined

-- really should be ⊳Π, but Π is a letter...
(⊳→) = flip mpk'
