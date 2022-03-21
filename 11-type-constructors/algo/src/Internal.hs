{-# LANGUAGE TypeFamilies #-}

module Internal where

import Common
import Control.Exception.Base
import qualified External as E

-- The internal type language
-- (following ``convention'', we speak of terms and types, not types and kinds
-- in the context of normalization)
-- TODO: In the future, should contain kind casts for a full gradual
-- language at the type level (hence elaboration)
-- NOTE: This is the same as the external langauge right now
data Term
  = TVar TID
  | Bse
  | Term :⊕ Term
  | ETHole HID
  | NETHole HID Term
  | Tλ TID Typ Term
  | TAp Term Term
  deriving (Show)

-- internal kind language
data Typ
  = Type
  | KHole
  | S Typ Term
  | Π TID Typ Typ
  deriving (Show)

-- NOTE: copy/pasted from External + s///, since Internal ≡ External right now
instance Eq Term where
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

instance Eq Typ where
  Type == Type = True
  KHole == KHole = True
  (S κ1 τ1) == (S κ2 τ2) = (κ1 == κ2) && (τ1 == τ2)
  (Π t κ1 κ2) == (Π t' κ3 κ4) =
    (κ1 == κ3) &&
    let t'' = fresh2 t t'
     in ((αRename t'' t κ2) == (αRename t'' t' κ4))
  _ == _ = False

instance Rewrite Term where
  type RW Term = Term
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

instance Rewrite Typ where
  type RW Typ = Term
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
