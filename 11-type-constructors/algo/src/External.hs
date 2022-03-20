{-# LANGUAGE TypeFamilies #-}

module External where

import Common
import Control.Exception.Base

-- The external type language
data Typ
  = TVar TID
  | Bse -- We'll remove this once built in types work
  | Typ :⊕ Typ
  | ETHole HID
  | NETHole HID Typ
  | Tλ TID Knd Typ
  | TAp Typ Typ
  deriving (Show)

data Knd
  -- the external kind language
  = Type
  | KHole
  | S Knd Typ
  | Π TID Knd Knd
  deriving (Show)

-- I'm afraid that sooner or later I'll run into a case where just using De
-- Bruijn indices would've been much easier
instance Eq Typ where
  (TVar t) == (TVar t') = t == t'
  Bse == Bse = True
  (τ1 :⊕ τ2) == (τ3 :⊕ τ4) = (τ1 == τ3) && (τ2 == τ4)
  (ETHole u) == (ETHole u') = u == u'
  (NETHole u τ) == (NETHole u' τ') = assert (τ == τ') $ u == u'
  (Tλ t κ τ) == (Tλ t' κ' τ') =
    κ == κ' &&
    let t'' = fresh2 t t'
     in (αRename t'' t τ) == (αRename t'' t' τ')
  (TAp τ1 τ2) == (TAp τ3 τ4) = (τ1 == τ3) && (τ2 == τ4)
  _ == _ = False

instance Eq Knd where
  Type == Type = True
  KHole == KHole = True
  (S κ1 τ1) == (S κ2 τ2) = (κ1 == κ2) && (τ1 == τ2)
  (Π t κ1 κ2) == (Π t' κ3 κ4) =
    (κ1 == κ3) &&
    let t'' = fresh2 t t'
     in ((αRename t'' t κ2) == (αRename t'' t' κ4))
  _ == _ = False

instance Rewrite Typ where
  type RW Typ = Typ
  αRename t'' t' (TVar t)
    | t' == t = TVar t''
    | otherwise = TVar t
  αRename t'' t' (τ1 :⊕ τ2) = (αRename t'' t' τ1) :⊕ (αRename t'' t' τ2)
  αRename t'' t' (NETHole u τ) = NETHole u (αRename t'' t' τ)
  αRename t'' t' (Tλ t κ τ)
    | t' == t = Tλ t'' (αRename t'' t' κ) (αRename t'' t' τ)
    | otherwise = Tλ t (αRename t'' t' κ) (αRename t'' t' τ)
  αRename t'' t' (TAp τ1 τ2) = TAp (αRename t'' t' τ1) (αRename t'' t' τ2)
  αRename _ _ τ = τ
  ----------
  subst τ' t' τ@(TVar t)
    | t' == t = τ'
    | otherwise = τ
  subst _ _ Bse = Bse
  subst τ' t' (τ1 :⊕ τ2) = (subst τ' t' τ1) :⊕ (subst τ' t' τ2)
  subst _ _ τ@(ETHole _) = τ
  subst τ' t' (NETHole u τ1) = NETHole u (subst τ' t' τ1)
  subst τ' t' (Tλ t κ τ1)
    | t' == t = error "Why are you doing this?"
    | otherwise = Tλ t (subst τ' t' κ) (subst τ' t' τ1)
  subst τ' t' (TAp τ1 τ2) = TAp (subst τ' t' τ1) (subst τ' t' τ2)

-- NOTE: What is this?
-- tequiv' aΓ (TAp τ1 τ2) τ3 κ = tequiv aΓ (βReduce τ1 τ2) τ3 κ
-- tequiv' aΓ τ1 (TAp τ2 τ3) κ = tequiv aΓ τ1 (βReduce τ2 τ3) κ
instance Rewrite Knd where
  type RW Knd = Typ
  αRename t'' t' (Π t κ1 κ2)
    | t' == t = Π t'' (αRename t'' t' κ1) (αRename t'' t' κ2)
    | otherwise = Π t (αRename t'' t' κ1) (αRename t'' t' κ2)
  αRename t'' t' (S κ τ) = S (αRename t'' t' κ) (αRename t'' t' τ)
  αRename _ _ κ = κ
  subst _ _ Type = Type
  subst _ _ KHole = KHole
  subst τ' t' (S κ τ) = S (subst τ' t' κ) (subst τ' t' τ)
  subst τ' t' (Π t κ1 κ2)
    | t' == t = error "Seriously, why are you doing this?"
    | otherwise = Π t (subst τ' t' κ1) (subst τ' t' κ2)
