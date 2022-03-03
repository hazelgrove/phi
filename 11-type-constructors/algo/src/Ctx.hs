module Ctx where

import Common

import Debug.Trace

data Ctx
  = Nil
  | Ctx :⌢ TAssump
  | Ctx :⌢⌢ HAssump

lookupT :: Ctx -> TID -> Maybe Knd
lookupT Nil _ = Nothing
lookupT (aΓ :⌢ (t', κ)) t
  | t' == t = Just κ
  | otherwise = lookupT aΓ t
lookupT (aΓ :⌢⌢ _) t = lookupT aΓ t

lookupH :: Ctx -> HID -> Maybe Knd
lookupH Nil _ = Nothing
lookupH (aΓ :⌢ _) u = lookupH aΓ u
lookupH (aΓ :⌢⌢ (u', κ)) u
  | u' == u = Just κ
  | otherwise = lookupH aΓ u

(⌢) :: Ctx -> TAssump -> Ctx
(⌢) aΓ tassump@(t, _κ) =
  case lookupT aΓ t of
    Just _ -> error "Do not shadow"
    Nothing -> aΓ :⌢ tassump

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
      Tλ t κ τ -> canon aΓ (subst ωτ2 t τ)
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
