{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
#define LINE (show (__LINE__ :: Integer))
module Algo
  ( module Algo
  , module Common
  , module Ctx
  ) where

import Common
import Ctx

import Control.Exception.Base
import Data.Kind qualified as DK
import Data.Maybe
import Debug.Trace

class Canon a
  -- C for Canonical
  where
  data C a :: DK.Type
  canon :: Ctx -> a -> Maybe (C a)

-- C Typ only has variables if they are base types (do not have singleton kind)
-- TAp s are β reduced as much as possible
-- Tλ s are ``values'' (we don't canon the body)
instance Canon Typ where
  newtype C Typ = CTyp{getTyp :: Typ}
                  deriving (Eq, Show, Rewrite)
  canon aΓ τ@(TVar t) = do
    γκ <- lookupT aΓ t
    case γκ of
      S κ' τ' -> canon aΓ τ'
      Type -> Just . CTyp $ τ
      KHole -> error "This should not happen"
      Π _ _ _ -> do
        ηExpand aΓ τ
  canon _ Bse = Just . CTyp $ Bse
  canon aΓ (τ1 :⊕ τ2) = do
    ωτ1 <- canon aΓ τ1
    ωτ2 <- canon aΓ τ2
    (wfak aΓ (getTyp ωτ1) Type) && (wfak aΓ (getTyp ωτ2) Type) &>> Just . CTyp $ getTyp ωτ1 :⊕ getTyp ωτ2
  canon aΓ τ@(ETHole u) = do
    _ <- lookupH aΓ u
    Just . CTyp $ τ
  canon aΓ τ@(NETHole u τ') = do
    κ <- lookupH aΓ u
    Just . CTyp $ τ
  canon aΓ (Tλ t κ τ) = do
    ωκ <- canon aΓ κ
    Just . CTyp $ Tλ t (getKnd ωκ) τ
  canon aΓ (TAp τ1 τ2) = do
    ωτ1 <- canon aΓ τ1
    ωτ2 <- canon aΓ τ2
    -- check κ (subkinding)
    case getTyp ωτ1 of
      Tλ t κ τ -> wfak aΓ (getTyp ωτ2) κ &>> canon aΓ (subst (getTyp ωτ2) t τ)
      _ -> trace ("Can't β-reduce " ++ (show $ TAp (getTyp ωτ1) (getTyp ωτ2))) $ Nothing

-- need a canonical form to normalize higher order singletons
instance Canon Knd where
  newtype C Knd = CKnd{getKnd :: Knd}
                  deriving (Eq, Show, Rewrite)
  canon aΓ Type = return Type
  canon aΓ KHole = return KHole
  canon aΓ (S κ τ) = do
    ωκ <- canon aΓ κ
    ωτ <- canon aΓ τ
    trace ("\nωκ: " ++ show ωκ ++ "\nωτ: " ++ show ωτ) $
      case ωκ of
        Type -> wfak aΓ ωτ Type &>> (Just $ S Type ωτ)
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

ηExpand :: _
ηExpand = undefined

tequiv :: Ctx -> Typ -> Typ -> Knd -> Bool
tequiv aΓ τ1 τ2 κ =
  isJust
    (do ωτ1 <- canon aΓ τ1
        ωτ2 <- canon aΓ τ2
        ωκ <- canon aΓ κ
        tequiv' aΓ ωτ1 ωτ2 ωκ &>> Just ())

tequiv' :: Ctx -> C Typ -> C Typ -> Knd -> Bool
tequiv' _ Bse Bse Type = True
tequiv' aΓ (ωτ1 :⊕ ωτ2) (ωτ3 :⊕ ωτ4) Type =
  (tequiv aΓ ωτ1 ωτ3 Type) && (tequiv aΓ ωτ2 ωτ4 Type)
tequiv' aΓ (ETHole u) (ETHole u') κ = isJust (do u == u' &>> lookupH aΓ u)
tequiv' aΓ (NETHole u1 τ1) (NETHole u2 τ2) κ =
  isJust
    (do _ <- u1 == u2 &>> lookupH aΓ u1
        assert (τ1 ≡ τ2) $ Just ())
tequiv' aΓ ωτ1@(Tλ _ _ _) ωτ2@(Tλ _ _ _) (Π t κ1 κ2) =
  tequiv (aΓ ⌢ (t, κ1)) (TAp ωτ1 $ TVar t) (TAp ωτ2 $ TVar t) κ2
tequiv' aΓ ωτ1 ωτ2 (S ωκ ωτ3) = tequiv aΓ ωτ1 ωτ2 ωκ && tequiv aΓ ωτ1 ωτ3 ωκ
tequiv' _ _ _ _ = False

kequiv :: Ctx -> Knd -> Knd -> Bool
kequiv aΓ κ1 κ2 =
  isJust
    (do ωκ1 <- canon aΓ κ1
        ωκ2 <- canon aΓ κ2
        kequiv' aΓ ωκ1 ωκ2 &>> Just ())

kequiv' :: Ctx -> Knd -> Knd -> Bool
kequiv' aΓ κ@(Π t _ _) κ'@(Π t' _ _) =
  let t'' = fresh2 t t'
   in let (Π _ κ1 κ2) = αRename t'' t κ
       in let (Π _ κ3 κ4) = αRename t'' t' κ'
           in (kequiv aΓ κ1 κ3) && (kequiv (aΓ ⌢ (t'', κ1)) κ2 κ4)
kequiv' aΓ (S κ1 τ1) (S κ2 τ2) = (kequiv aΓ κ1 κ2) && (tequiv aΓ τ1 τ2 κ1)
kequiv' _ κ1 κ2 = κ1 ≡ κ2

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
  wfak aΓ ωτ1 Type &>> Just ()
  wfak aΓ ωτ2 Type &>> Just ()
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
               kequiv
               aΓ
               νκ'
               κ ||
             csk aΓ νκ κ) &>>
            Just ()
          _ -> error "pk s are always singletons")

csk :: Ctx -> Knd -> Knd -> Bool
csk aΓ κ κ' =
  isJust
    (do ωκ1 <- trace ("\n" ++ LINE ++ "\n") $ canon aΓ κ
        ωκ2 <- trace ("\n" ++ LINE ++ "\n") $ canon aΓ κ'
        case (ωκ1, ωκ2) of
          (_, KHole) -> Just ()
          (KHole, _) -> Just ()
          (Π t κ1 κ2, Π t' κ3 κ4) ->
            ((csk aΓ κ3 κ1) && (csk (aΓ ⌢ (t, κ3)) κ2 (αRename t t' κ4))) &>>
            Just ()
          (S Type τ, Type) ->
            trace "\nYou should never see this unless you call csk directly\n" $
            Just ()
          _ -> kequiv aΓ ωκ1 ωκ2 &>> Just ())
