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
  iδ <- (ana_elab' iΓ eτ iτ).term -- shouldn't have holes in initial ctx
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
  AER{term = iδ1, ..} <- ana_elab' iΓ eτ1 Type
  AER{term = iδ2, ..} <- ana_elab' iΓ eτ2 Type
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
  AER{term = iδ1, ..} <- ana_elab' iΓ eτ1 (Π tπ iτIn iτOut)
  AER{term = iδ2, ..} <- ana_elab' iΓ eτ2 (iτIn)
  return $ SER (subst iδ2 tπ iτOut) (TAp iδ1 iδ2) iΓ

data AnaElabResult =
  AER
    { term :: Term
    , iΓ :: Ctx -- named for shadowing
    }

-- NOTE: prob don't need ana_elab :: ECtx.Ctx
-- TODO: something something not holes
ana_elab' :: Ctx -> E.Typ -> Typ -> Maybe AnaElabResult
ana_elab' iΓ (E.ETHole u) iτ = assert (isNothing $ lookupH iΓ u) $
  return $ AER (ETHole u) (iΓ :⌢⌢ (u, iτ))
ana_elab' iΓ (E.NETHole u eτ) iτ = assert (isNothing $ lookupH iΓ u) $ do
  SER{term = iδ, ..} <- syn_elab' iΓ eτ
  return $ AER (NETHole u iδ) (iΓ :⌢⌢ (u, iτ))
ana_elab' iΓ eτ iτ = do
  SER {typ = iτ', term = iδ, ..} <- syn_elab' iΓ eτ
  (iΓ |- (iτ' ≲ iτ)) &>> (return $ AER iδ iΓ)

τ_elab :: Ctx -> E.Knd -> Maybe Typ
τ_elab = undefined

wh_reduc = undefined

wh_normal = undefined

term_normal = undefined

type_normal = undefined

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
