module ECtx where

import Common
import External

type EAssump = (TID, Typ)

type TAssump = (TID, Knd)

data Ctx
  = Nil
  | Ctx :⌢ TAssump
  | Ctx :⌢⌢⌢ EAssump
  deriving (Show)

lookupT :: Ctx -> TID -> Maybe Knd
lookupT Nil _ = Nothing
lookupT (aΓ :⌢ (t', κ)) t
  | t' == t = Just κ
  | otherwise = lookupT aΓ t
lookupT (aΓ :⌢⌢⌢ _) t = lookupT aΓ t

lookupE :: Ctx -> TID -> Maybe Typ
lookupE Nil _ = Nothing
lookupE (iΓ :⌢ _) t = lookupE iΓ t
lookupE (iΓ :⌢⌢⌢ (t', τ)) t
  | t' == t = Just τ
  | otherwise = lookupE iΓ t

(⌢) :: Ctx -> TAssump -> Ctx
(⌢) aΓ tassump@(t, _κ) =
  case lookupT aΓ t of
    Just _ -> error "Do not shadow"
    Nothing -> aΓ :⌢ tassump

(⌢⌢⌢) :: Ctx -> EAssump -> Ctx
(⌢⌢⌢) aΓ eassump@(x, _τ) =
  case lookupE aΓ x of
    Just _ -> error "Do not shadow"
    Nothing -> aΓ :⌢⌢⌢ eassump
