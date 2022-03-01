-- NOTE: we alwys assume programs are α-renamed so no shadowing occurs
module Common where

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
  deriving (Eq, Show)

class Rewrite a
  {-
   - okay, not exactly an alpha-conversion,
   - but replaces all instances of a variable with another
   -}
  where
  αRename :: TID -> TID -> a -> a

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

instance Rewrite Knd where
  αRename t'' t' (Π t κ1 κ2)
    | t' == t = Π t'' (αRename t'' t' κ1) (αRename t'' t' κ2)
    | otherwise = Π t (αRename t'' t' κ1) (αRename t'' t' κ2)
  αRename t'' t' (S κ τ) = S (αRename t'' t' κ) (αRename t'' t' τ)
  αRename _ _ κ = κ
