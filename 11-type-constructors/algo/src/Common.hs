-- NOTE: we alwys assume programs are α-renamed so no shadowing occurs
module Common where

import Control.Monad (MonadPlus, mzero)
import Debug.Trace
import Language.Perl
import System.IO.Unsafe

type TID = String

type HID = Int

type TAssump = (TID, Knd)

type HAssump = (HID, Knd)

data Typ
  = TVar TID
  | Bse
  | Typ :⊕ Typ
  | ETHole HID
  | NETHole HID Typ
  | Tλ TID Knd Typ
  | TAp Typ Typ
  deriving (Eq, Show)

data Knd
  = Type
  | KHole
  | S Knd Typ
  | Π TID Knd Knd
  deriving (Show)

-- I'm afraid that sooner or later I'll run into a case where just using De
-- Bruijn indeces would've been much easier
instance Eq Knd where
  Type == Type = True
  KHole == KHole = True
  (S κ1 τ1) == (S κ2 τ2) = (κ1 == κ2) && (τ1 == τ2)
  (Π t κ1 κ2) == (Π t' κ3 κ4) = (κ1 == κ3) && (κ3 == (αRename t' t κ4))
  _ == _ = False

class Rewrite a
  {-
   - okay, not exactly an alpha-conversion,
   - but replaces all instances of a variable with another
   -}
  where
  αRename :: TID -> TID -> a -> a
  subst :: Typ -> TID -> a -> a

instance Rewrite Typ where
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

-- tequiv' aΓ (TAp τ1 τ2) τ3 κ = tequiv aΓ (βReduce τ1 τ2) τ3 κ
-- tequiv' aΓ τ1 (TAp τ2 τ3) κ = tequiv aΓ τ1 (βReduce τ2 τ3) κ
instance Rewrite Knd where
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

(|>>) :: MonadPlus m => Bool -> m a -> m a
(|>>) True = id
(|>>) False = \_ -> mzero

fresh :: TID -> TID
fresh t =
  unsafePerformIO $
  withPerl $
  eval $
  "my $tid = '" ++
  t ++
  "';" ++
  "$tid =~ /(\\D+)(\\d+)?/;" ++
  "if(defined $2){" ++
  "$_ = $1 . ($2 + 1)" ++ "} else{" ++ "$_ = $1 . '1'" ++ "}"
