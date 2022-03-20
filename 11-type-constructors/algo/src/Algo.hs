module Algo
  ( module Algo
  , module ECtx
  ) where

import Common
import qualified ECtx
import qualified External as E
import ICtx
import Internal

import Control.Exception.Base
import Data.Maybe
import Debug.Trace

δsyn_elab :: Ctx -> E.Typ -> Maybe (Typ, Term)
δsyn_elab aΓ (E.TVar x) = do
  undefined

τ_elab :: Ctx -> E.Knd -> Maybe Typ
τ_elab = undefined

wh_reduc = undefined

wh_normal = undefined

term_normal = undefined

type_normal = undefined

tequiv :: ECtx.Ctx -> E.Typ -> E.Typ -> E.Knd -> Bool
tequiv = undefined
