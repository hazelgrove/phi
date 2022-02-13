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
  | NETHole
  deriving (Eq, Show)

data Knd
  = Type
  | KHole
  | S Knd Typ
  | Π TID Knd Knd
  deriving (Eq, Show)
