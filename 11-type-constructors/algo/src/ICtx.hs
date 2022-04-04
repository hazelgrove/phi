module ICtx where

import Common
import Internal

type TAssump = (TID, Typ)

type HAssump = (HID, Typ)

data Ctx
  = Nil
  | Ctx :⌢ TAssump
  | Ctx :⌢⌢ HAssump
  deriving (Show)

-- TODO: this is c/p from ECtx
-- rename variables (since we're talking about Terms/Types now)
lookupT :: Ctx -> TID -> Maybe Typ
lookupT Nil _ = Nothing
lookupT (aΓ :⌢ (t', κ)) t
  | t' == t = Just κ
  | otherwise = lookupT aΓ t
lookupT (aΓ :⌢⌢ _) t = lookupT aΓ t

removeT :: Ctx -> TID -> Ctx
removeT (iΓ :⌢ (t', τ)) t
  | t' == t = iΓ
  | otherwise = (removeT iΓ t) ⌢ (t', τ)
removeT (iΓ :⌢⌢ (u, τ)) t = (removeT iΓ t) ⌢⌢ (u, τ)

lookupH :: Ctx -> HID -> Maybe Typ
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

(⌢⌢) :: Ctx -> HAssump -> Ctx
(⌢⌢) aΓ hassump@(u, _κ) =
  case lookupH aΓ u of
    Just _ -> error "Do not shadow"
    Nothing -> aΓ :⌢⌢ hassump
