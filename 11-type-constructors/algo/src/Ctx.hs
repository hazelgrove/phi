module Ctx where

import Common

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

instance Canon Typ where
  canon _ _ = undefined

instance Canon Knd where
  canon _ _ = undefined
