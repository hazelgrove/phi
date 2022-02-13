module Algo (module Algo, module Common, module Ctx) where

import Common
import Ctx

tequiv :: Ctx -> Typ -> Typ -> Knd -> Bool
-- tequiv aΓ τ1 τ2 κ = True
tequiv aΓ (τ1 :⊕ τ2) (τ3 :⊕ τ4) Type = (tequiv aΓ τ1 τ3 Type) && (tequiv aΓ τ2 τ4 Type)
tequiv aΓ τ1 τ2 (Π t κ1 κ2) = error "unimplemented"
tequiv aΓ τ1 τ2 κ
  | τ1 == τ2 && True = True
  | otherwise = False

kequiv :: Ctx -> Knd -> Knd -> Bool
kequiv aΓ (Π t κ1 κ2) (Π t' κ3 κ4) = t == t' && (kequiv aΓ κ1 κ3) && (kequiv (aΓ ⌢ (t, κ1)) κ2 κ4)
kequiv aΓ (S κ1 τ1) (S κ2 τ2) =  (kequiv aΓ κ1 κ2) && (tequiv aΓ τ1 τ2 κ1)
kequiv aΓ κ1 κ2
  | κ1 == κ2 && True = True
  | otherwise = False
