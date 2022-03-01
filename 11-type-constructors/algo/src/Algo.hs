module Algo
  ( module Algo
  , module Common
  , module Ctx
  ) where

import Common
import Ctx
import Data.Maybe

tequiv :: Ctx -> Typ -> Typ -> Knd -> Bool
tequiv aΓ τ1 τ2 κ =
  isJust
    (do ωτ1 <- canon aΓ τ1
        ωτ2 <- canon aΓ τ2
        ωκ <- canon aΓ κ
        if (tequiv' aΓ ωτ1 ωτ2 ωκ)
          then Just True
          else Nothing)

tequiv' :: Ctx -> Typ -> Typ -> Knd -> Bool
tequiv' _ Bse Bse Type = True
tequiv' aΓ (τ1 :⊕ τ2) (τ3 :⊕ τ4) Type =
  (tequiv aΓ τ1 τ3 Type) && (tequiv aΓ τ2 τ4 Type)
tequiv' aΓ τ1@(Tλ _ _ _) τ2@(Tλ _ _ _) (Π t κ1 κ2) =
  tequiv (aΓ ⌢ (t, κ1)) (TAp τ1 $ TVar t) (TAp τ2 $ TVar t) κ2
-- need to check κ v. lookupH
tequiv' aΓ (ETHole u) (ETHole u') κ = u == u' && isJust (lookupH aΓ u)
tequiv' aΓ (NETHole u1 τ1) (NETHole u2 τ2) κ =
  u1 == u2 && isJust (lookupH aΓ u1)
tequiv' _ _ _ _ = False

kequiv :: Ctx -> Knd -> Knd -> Bool
kequiv aΓ (Π t κ1 κ2) κ'@(Π t' _ _) =
  let (Π t'' κ3 κ4) = αRename t t' κ'
   in (kequiv aΓ κ1 κ3) && (kequiv (aΓ ⌢ (t, κ1)) κ2 κ4)
kequiv aΓ (S κ1 τ1) (S κ2 τ2) = (kequiv aΓ κ1 κ2) && (tequiv aΓ τ1 τ2 κ1)
kequiv _ κ1 κ2
  | κ1 == κ2 = undefined
  | otherwise = False

βReduce :: Typ -> Typ -> Typ
βReduce (Tλ t κ τ) τ'
  | True = subst τ' t τ
βReduce τ1 τ2 = error $ "Can't β-reduce " ++ (show $ TAp τ1 τ2)
