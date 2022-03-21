module ECtx where

import Common
import External

type TAssump = (TID, Knd)

data Ctx
  = Nil
  | Ctx :⌢ TAssump

lookupT :: Ctx -> TID -> Maybe Knd
lookupT Nil _ = Nothing
lookupT (aΓ :⌢ (t', κ)) t
  | t' == t = Just κ
  | otherwise = lookupT aΓ t

(⌢) :: Ctx -> TAssump -> Ctx
(⌢) aΓ tassump@(t, _κ) =
  case lookupT aΓ t of
    Just _ -> error "Do not shadow"
    Nothing -> aΓ :⌢ tassump
