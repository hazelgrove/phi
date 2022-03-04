{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE CPP #-}
#define LINE (show (__LINE__ :: Integer))
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
  | κ1 ≡ κ2 = undefined
  | otherwise = False

class Canon a where
  canon :: Ctx -> a -> Maybe a

instance Canon Typ where
  canon aΓ (TVar t) = do
    κ <- lookupT aΓ t
    case κ of
      S κ' τ' -> canon aΓ τ'
      Type -> Just $ TVar t
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
      Tλ t κ τ -> wfak aΓ ωτ2 κ |>> canon aΓ (subst ωτ2 t τ)
      _ -> trace ("Can't β-reduce " ++ (show $ TAp ωτ1 ωτ2)) Nothing

instance Canon Knd where
  canon aΓ Type = return Type
  canon aΓ KHole = return KHole
  canon aΓ (S κ τ) = do
    ωκ <- canon aΓ κ
    ωτ <- canon aΓ τ
    trace ("\nωκ: " ++ show ωκ ++ "\nωτ: " ++ show ωτ) $
      case ωκ of
        Type -> wfak aΓ ωτ Type |>> (Just $ S Type ωτ)
        KHole -> canon aΓ $ S KHole ωτ
        S κ' τ' -> canon aΓ $ S κ' τ'
        Π t κ1 κ2 ->
          let t' = fresh t
           in let ωκ' = Π t' (αRename t' t κ1) (S κ2 (TAp ωτ $ TVar t'))
               in trace ("\nNormalize Singleton: " ++ show ωκ') $ canon aΓ ωκ'
  canon aΓ (Π t κ1 κ2) = do
    ωκ1 <- canon aΓ κ1
    ωκ2 <- canon (aΓ ⌢ (t, κ1)) κ2
    Just $ Π t ωκ1 ωκ2

pk :: Ctx -> Typ -> Maybe Knd
pk aΓ τ = do
  ωτ <- canon aΓ τ
  pk' aΓ ωτ

pk' :: Ctx -> Typ -> Maybe Knd
pk' aΓ (TVar t) = do
  κ <- lookupT aΓ t
  Just $ S κ (TVar t)
pk' aΓ Bse = Just $ S Type Bse
pk' aΓ (ωτ1 :⊕ ωτ2) = do
  wfak aΓ ωτ1 Type |>> Just ()
  wfak aΓ ωτ2 Type |>> Just ()
  Just $ S Type (ωτ1 :⊕ ωτ2)
pk' aΓ τ@(ETHole u) = do
  κ <- lookupH aΓ u
  Just $ S κ τ
pk' aΓ τ@(NETHole u _) = do
  κ <- lookupH aΓ u
  Just $ S κ τ
pk' aΓ τ'@(Tλ t κ τ) = do
  κ' <- pk' (aΓ ⌢ (t, κ)) τ
  Just $ S (Π "t" κ κ') τ'
pk' _ _ = error "Typ should be canonized coming in"

wfak :: Ctx -> Typ -> Knd -> Bool
wfak aΓ τ κ =
  isJust
    (do νκ <- pk aΓ τ
        ωκ <- canon aΓ κ
        case νκ of
          S νκ' ντ'
          -- the short circuit cuts a circular dependency between wfak and csk
           ->
            (trace
               ("\nshort circuit: " ++ show νκ ++ ", " ++ show κ ++ "\n")
               νκ' ≡
             κ ||
             csk aΓ νκ κ) |>>
            Just ()
          _ -> error "pk s are always singletons")

-- TODO: a lot
csk :: Ctx -> Knd -> Knd -> Bool
csk aΓ κ κ' =
  isJust
    (do ωκ1 <- trace ("\n" ++ LINE ++ "\n") $ canon aΓ κ
        ωκ2 <- trace ("\n" ++ LINE ++ "\n") $ canon aΓ κ'
        case (ωκ1, ωκ2) of
          (_, KHole) -> Just ()
          (KHole, _) -> Just ()
          (Π t κ1 κ2, Π t' κ3 κ4) ->
            ((csk aΓ κ3 κ1) && (csk (aΓ ⌢ (t, κ3)) κ2 (αRename t t' κ4))) |>>
            Just ()
          (S Type τ, Type) ->
            trace "\nYou should never see this unless you call csk directly\n" $
            Just ()
          _ -> ωκ1 ≡ ωκ2 |>> Just ())
