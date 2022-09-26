module ICtx where

import Common
import Internal

type EAssump = (TID, Term)

type TAssump = (TID, Typ)

type HAssump = (HID, Typ)

data Ctx
  = Nil
  | Ctx :⌢ TAssump
  | Ctx :⌢⌢ HAssump
  | Ctx :⌢⌢⌢ EAssump
  deriving (Show)

-- TODO: this is c/p from ECtx
-- rename variables (since we're talking about Terms/Types now)
lookupE :: Ctx -> TID -> Maybe Term
lookupE Nil _ = Nothing
lookupE (iΓ :⌢ _) t = lookupE iΓ t
lookupE (iΓ :⌢⌢ _) t = lookupE iΓ t
lookupE (iΓ :⌢⌢⌢ (t', δ)) t
  | t' == t = Just δ
  | otherwise = lookupE iΓ t

lookupT :: Ctx -> TID -> Maybe Typ
lookupT Nil _ = Nothing
lookupT (aΓ :⌢ (t', κ)) t
  | t' == t = Just κ
  | otherwise = lookupT aΓ t
lookupT (aΓ :⌢⌢ _) t = lookupT aΓ t
lookupT (iΓ :⌢⌢⌢ _) t = lookupT iΓ t

removeT :: Ctx -> TID -> Ctx
removeT Nil _ = error "Can't remove from an empty ctx"
removeT (iΓ :⌢ (t', τ)) t
  | t' == t = iΓ
  | otherwise = (removeT iΓ t) ⌢ (t', τ)
removeT (iΓ :⌢⌢ (u, τ)) t = (removeT iΓ t) ⌢⌢ (u, τ)
removeT (iΓ :⌢⌢⌢ (u, τ)) t = (removeT iΓ t) ⌢⌢⌢ (u, τ)

lookupH :: Ctx -> HID -> Maybe Typ
lookupH Nil _ = Nothing
lookupH (aΓ :⌢ _) u = lookupH aΓ u
lookupH (aΓ :⌢⌢ (u', κ)) u
  | u' == u = Just κ
  | otherwise = lookupH aΓ u
lookupH (iΓ :⌢⌢⌢ _) t = lookupH iΓ t

(⌢) :: Ctx -> TAssump -> Ctx
(⌢) aΓ tassump@(t, _κ) =
  case lookupT aΓ t of
    Just _ -> error "Do not shadow"
    Nothing -> aΓ :⌢ tassump

(⌢⌢) :: Ctx -> HAssump -> Ctx
(⌢⌢) aΓ hassump@(u, _κ) =
  case lookupH aΓ u of
    Just _ -> error "Do not shadow"
    Nothing -> aΓ :⌢⌢ hassump

(⌢⌢⌢) :: Ctx -> EAssump -> Ctx
(⌢⌢⌢) iΓ eassump@(x, _δ) =
  case lookupE iΓ x of
    Just _ -> error "Do not shadow"
    Nothing -> iΓ :⌢⌢⌢ eassump
