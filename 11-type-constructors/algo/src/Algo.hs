{-# LANGUAGE PartialTypeSignatures #-}

module Algo
  ( module Algo
  , module Common
  , module Ctx
  ) where

import Common
import Ctx

import Data.Maybe
import Debug.Trace

tequiv :: Ctx -> Typ -> Typ -> Knd -> Bool
tequiv aΓ τ1 τ2 κ =
  isJust
    (do ωτ1 <- canon aΓ τ1
        ωτ2 <- canon aΓ τ2
        ωκ <- canon aΓ κ
        tequiv' aΓ ωτ1 ωτ2 ωκ |>> Just ())

-- (didn't define a seperate datatype since more symbols would clash and I'm
-- still changing a lot of stuff)
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

class Canon a where
  canon :: Ctx -> a -> Maybe a
  canon' :: a -> Ctx -> Maybe a
  canon' = flip canon

instance Canon Typ where
  canon aΓ (TVar t) = do
    κ <- lookupT aΓ t
    case κ of
      S κ' τ' -> canon aΓ τ'
      Type -> error "We aren't handling builtins yet"
      KHole -> error "This should not happen"
      Π t' κ1 κ2 -> error "???"
  canon _ Bse = Just Bse
  canon aΓ (τ1 :⊕ τ2) = do
    ωτ1 <- canon aΓ τ1
    ωτ2 <- canon aΓ τ2
    return $ ωτ1 :⊕ ωτ2
  canon aΓ τ@(ETHole u) = do
    κ <- lookupH aΓ u
    return τ
  canon aΓ τ@(NETHole u τ') = do
    κ <- lookupH aΓ u
    return τ
  canon aΓ (Tλ t κ τ) = do
    ωκ <- canon aΓ κ
    return $ Tλ t ωκ τ
  canon aΓ (TAp τ1 τ2) = do
    ωτ1 <- canon aΓ τ1
    ωτ2 <- canon aΓ τ2
    -- check κ (subkinding)
    case ωτ1 of
      Tλ t κ τ -> wfak aΓ ωτ1 κ |>> canon aΓ (subst ωτ2 t τ)
      _ -> trace ("Can't β-reduce " ++ (show $ TAp ωτ1 ωτ2)) Nothing

instance Canon Knd where
  canon aΓ Type = return Type
  canon aΓ KHole = return KHole
  canon aΓ (S κ τ) = do
    ωκ <- canon aΓ κ
    ωτ <- canon aΓ τ
    return $ S ωκ ωτ
  canon aΓ (Π t κ1 κ2) = do
    ωκ1 <- canon aΓ κ1
    ωκ2 <- canon aΓ κ2
    return $ Π t ωκ1 ωκ2

pk :: Ctx -> Typ -> Knd
pk aΓ τ = undefined

wfak :: Ctx -> Typ -> Knd -> Bool
wfak aΓ τ κ = csk aΓ (pk aΓ τ) κ

csk :: Ctx -> Knd -> Knd -> Bool
csk aΓ κ1 κ2 = undefined
