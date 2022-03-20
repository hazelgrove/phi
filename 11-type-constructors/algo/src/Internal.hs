module Internal where

import Ctx
import External (HAssump, HID, TAssump, TID, (&>>))
import qualified External as E

-- The internal type language
-- (following ``convention'', we speak of terms and types, not types and kinds
-- in the context of normalization)
-- TODO: In the future, should contain kind casts for a full gradual
-- language at the type level (hence elaboration)
data Term
  = TVar TID
  | Bse
  | Term :⊕ Term
  | ETHole HID
  | NETHole HID Term
  | Tλ TID Typ Term
  | TAp Term Term

-- internal kind language
data AtomicTyp
  = Type
  | KHole

data Typ
  = Atom AtomicTyp
  | S AtomicTyp Term
  | Π TID Typ Typ

δsyn_elab :: Ctx -> E.Typ -> Maybe (Typ, Term)
δsyn_elab aΓ (E.TVar x) = do
  γκ <- lookupT aΓ x



τ_elab :: Ctx -> E.Knd -> Maybe Typ
τ_elab = undefined

wh_reduc = undefined

wh_normal = undefined

term_normal = undefined

type_normal = undefined
