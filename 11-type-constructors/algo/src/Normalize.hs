{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Normalize where

import Common
import qualified ECtx
import qualified External as E
import ICtx
import Internal

import Control.Exception.Base
import Control.Monad
import Data.Maybe
import Debug.Trace

-- NOTE: ' marks internal functions
-- non ' functions are ``public facing''
-- non ' functions perform more checks/conversions before calling ' version
-- internal functions typically call internal functions
fixKnd' :: Ctx -> E.Knd -> Maybe Typ
fixKnd' _ E.Type = return Type
fixKnd' _ E.KHole = return KHole
fixKnd' iΓ (E.S eκ eτ) = do
  iτ <- fixKnd' iΓ eκ
  AER {term = iδ} <- ana_elab' iΓ eτ iτ -- shouldn't have holes in initial ctx
  return $ S iτ iδ
fixKnd' iΓ (E.Π t eκ1 eκ2) = do
  iτ1 <- fixKnd' iΓ eκ1
  iτ2 <- fixKnd' (iΓ ⌢ (t, iτ1)) eκ2
  return $ Π t iτ1 iτ2

fixCtx :: ECtx.Ctx -> Maybe Ctx
fixCtx ECtx.Nil = Just Nil
fixCtx (eΓ ECtx.:⌢ (t, eκ)) = do
  iΓ <- fixCtx eΓ
  iτ <- fixKnd' iΓ eκ
  return $ iΓ ⌢ (t, iτ)

data SynElabResult =
  SER
    { typ :: Typ
    , term :: Term
    , iΓ :: Ctx -- named for shadowing
    }
  deriving (Show)

syn_elab :: ECtx.Ctx -> E.Typ -> Maybe SynElabResult
syn_elab eΓ eτ = do
  iΓ <- fixCtx eΓ
  syn_elab' iΓ eτ

syn_elab' :: Ctx -> E.Typ -> Maybe SynElabResult
syn_elab' iΓ (E.TVar t)
  -- free variables should already be in a hole
 = do
  iτ <- lookupT iΓ t
  return $ SER (S iτ $ TVar t) (TVar t) iΓ
syn_elab' iΓ E.Bse = return $ SER (S Type Bse) Bse iΓ
syn_elab' iΓ (eτ1 E.:⊕ eτ2) = do
  AER {term = iδ1, ..} <- ana_elab' iΓ eτ1 Type
  AER {term = iδ2, ..} <- ana_elab' iΓ eτ2 Type
  return $ SER (S Type $ iδ1 :⊕ iδ2) (iδ1 :⊕ iδ2) iΓ
syn_elab' iΓ (E.ETHole u) =
  assert (isNothing $ lookupH iΓ u) $
  return $ SER (S KHole $ ETHole u) (ETHole u) (iΓ ⌢⌢ (u, KHole))
syn_elab' iΓ (E.NETHole u eτ) =
  assert (isNothing $ lookupH iΓ u) $ do
    SER {term = iδ, ..} <- syn_elab' iΓ eτ
    return $ SER (S KHole $ NETHole u iδ) (NETHole u iδ) (iΓ ⌢⌢ (u, KHole))
syn_elab' iΓ (E.Tλ t eκ eτ) = do
  iτ1 <- fixKnd' iΓ eκ
  SER {typ = iτ2, term = iδ, ..} <- syn_elab' (iΓ ⌢ (t, iτ1)) eτ
  return $ SER (S (Π t iτ1 iτ2) (Tλ t iτ1 iδ)) (Tλ t iτ1 iδ) (removeT iΓ t)
syn_elab' iΓ (E.TAp eτ1 eτ2) = do
  SER {typ = iτ1} <- syn_elab' iΓ eτ1 -- we don't have a plain syn
  MPKR {..} <- iΓ |- (iτ1 ⊳→)
  AER {term = iδ1, ..} <- ana_elab' iΓ eτ1 (Π tπ iτIn iτOut)
  AER {term = iδ2, ..} <- ana_elab' iΓ eτ2 (iτIn)
  return $ SER (subst iδ2 tπ iτOut) (TAp iδ1 iδ2) iΓ

data AnaElabResult =
  AER
    { term :: Term
    , iΓ :: Ctx -- named for shadowing
    }
  deriving (Show)

-- NOTE: prob don't need ana_elab :: ECtx.Ctx
-- TODO: something something not holes
ana_elab' :: Ctx -> E.Typ -> Typ -> Maybe AnaElabResult
ana_elab' iΓ (E.ETHole u) iτ =
  assert (isNothing $ lookupH iΓ u) $ return $ AER (ETHole u) (iΓ ⌢⌢ (u, iτ))
ana_elab' iΓ (E.NETHole u eτ) iτ =
  assert (isNothing $ lookupH iΓ u) $ do
    SER {term = iδ, ..} <- syn_elab' iΓ eτ
    return $ AER (NETHole u iδ) (iΓ ⌢⌢ (u, iτ))
ana_elab' iΓ eτ iτ = do
  SER {typ = iτ', term = iδ, ..} <- syn_elab' iΓ eτ
  (iΓ |- (iτ' ≲ iτ)) &>> (return $ AER iδ iΓ)

typ_elab' :: Ctx -> E.Knd -> Maybe Typ
typ_elab' _ E.Type = return Type
typ_elab' _ E.KHole = return KHole
typ_elab' iΓ (E.S eκ eτ) = do
  iτ <- typ_elab' iΓ eκ
  AER {term = iδ} <- ana_elab' iΓ eτ iτ
  return $ S iτ iδ
typ_elab' iΓ (E.Π t eκ1 eκ2) = do
  iτ1 <- typ_elab' iΓ eκ1
  iτ2 <- typ_elab' (iΓ ⌢ (t, iτ1)) eκ2
  return $ Π t iτ1 iτ2

is_path :: Term -> Bool
is_path Bse = True
is_path (TVar _) = True
is_path (_ :⊕ _) = True
is_path (ETHole _) = True
is_path (NETHole _ _) = True
is_path (TAp δ1 _) = is_path δ1
is_path _ = False

nat_type :: Ctx -> Term -> Typ
nat_type _ Bse = Type
nat_type iΓ (TVar t) =
  case lookupT iΓ t of
    Just τ -> τ
    Nothing -> error $ "free variable: " ++ t ++ "\n"
nat_type iΓ (δ1 :⊕ δ2) = Type
nat_type iΓ (ETHole u) =
  case lookupH iΓ u of
    Just τ -> τ
    Nothing -> error "free hole?\n"
nat_type iΓ (NETHole u _) =
  case lookupH iΓ u of
    Just τ -> τ
    Nothing -> error "free hole?\n"
nat_type _ (Tλ _ _ _) = error "not a path\n"
nat_type iΓ (TAp δ1 δ2) =
  case type_normal iΓ (nat_type iΓ δ1) of
    Π t τ1 τ2 -> subst δ2 t τ2
    _ -> error "unexpected natural type\n"

wh_path_reduc :: Ctx -> Term -> Either Term Term
wh_path_reduc iΓ δ =
  case nat_type iΓ δ of
    S _ δ' -> Right $ δ'
    _ -> Left $ δ

-- Maybe since ``otherwise''
wh_reduc :: Ctx -> Term -> Either Term Term
wh_reduc iΓ δ
  | is_path δ = wh_path_reduc iΓ δ
  | otherwise = wh_reduc' iΓ δ

wh_reduc' :: Ctx -> Term -> Either Term Term
wh_reduc' _ (TAp (Tλ t τ δ1) δ2) = assert (True) $ Right $ subst δ2 t δ1 {- check δ2 against τ -}
wh_reduc' iΓ δ@(TAp δ1 δ2) =
  case wh_reduc' iΓ δ1 of
    Right δ1' -> assert (δ1' /= δ1) $ Right (TAp δ1' δ2)
    Left ωδ1 -> assert (ωδ1 == δ1) $ Left δ
wh_reduc' _ δ = Left δ

wh_normal :: Ctx -> Term -> Term
wh_normal iΓ δ =
  case wh_reduc iΓ δ of
    Right δ' -> wh_normal iΓ δ'
    Left ωδ -> assert (ωδ == δ) $ δ

term_normal :: Ctx -> Term -> Typ -> Term
term_normal iΓ δ τ =
  let ωτ = type_normal iΓ τ
   in case ωτ of
        Type ->
          case path_normal iΓ (wh_normal iΓ δ) of
            (ωδ, Type) -> ωδ
            _ -> error "did not reduce enough?\n" -- csk
        KHole -> error "TODO: KHole\n"
        S Type sδ ->
          case path_normal iΓ (wh_normal iΓ δ) of
            (ωδ, Type) -> assert (ωδ ≡ sδ) $ ωδ
            _ -> error "error 3\n"
        S KHole sδ -> error "TODO\n"
        S _ _ -> error "type_normal failure?\n"
        Π t ωτ1 ωτ2 ->
          let t' = fresh t
           in Tλ t' ωτ1 (term_normal (iΓ ⌢ (t', ωτ1)) (TAp δ (TVar t')) ωτ2)

path_normal :: Ctx -> Term -> (Term, Typ)
path_normal _ Bse = (Bse, Type)
path_normal iΓ (TVar t) =
  case lookupT iΓ t of
    Just τ -> (TVar t, τ)
    Nothing -> error "free var again!\n"
path_normal iΓ (δ1 :⊕ δ2) =
  let ωδ1 = term_normal iΓ δ1 Type
   in let ωδ2 = term_normal iΓ δ2 Type
       in (ωδ1 :⊕ ωδ2, Type)
path_normal iΓ (ETHole u) =
  case lookupH iΓ u of
    Just τ -> (ETHole u, τ)
    Nothing -> error "free hole!\n"
path_normal iΓ (NETHole u δ) =
  case lookupH iΓ u of
    Just τ -> (NETHole u δ, τ)
    Nothing -> error "free hole!\n"
path_normal _ (Tλ _ _ _) = error "not a path!!\n"
path_normal iΓ (TAp δ1 δ2) =
  let (δ1', τ1) = path_normal iΓ δ1
   in case type_normal iΓ τ1 of
        Π t τ1' τ2' ->
          let δ2' = term_normal iΓ δ2 τ1
           in (TAp δ1' δ2', subst δ2' t τ2')
        _ -> error "unexpected path normal type\n"

type_normal :: Ctx -> Typ -> Typ
type_normal _ Type = Type
type_normal _ KHole = KHole
type_normal iΓ (S τ δ) =
  let ωτ = type_normal iΓ τ
   in let ωδ = term_normal iΓ δ ωτ
       in case ωτ of
            Type -> S Type ωδ
            KHole -> S KHole ωδ
            S τ' δ' -> type_normal iΓ (S τ' δ')
            Π t τ1 τ2 ->
              let t' = fresh t
               in let τ3 = αRename t' t τ2
                   in let ωδ' =
                            term_normal (iΓ ⌢ (t', τ1)) (TAp ωδ (TVar t')) τ3
                       in Π t' τ1 (S τ3 ωδ')
type_normal iΓ (Π t τ1 τ2) =
  let ωτ1 = type_normal iΓ τ1
   in Π t ωτ1 (type_normal (iΓ ⌢ (t, ωτ1)) τ2)

tequiv :: ECtx.Ctx -> E.Typ -> E.Typ -> E.Knd -> Bool
tequiv eΓ eτ1 eτ2 eκ =
  isJust
    (do iΓ <- fixCtx eΓ
        iτ <- typ_elab' iΓ eκ
        AER {term = iδ1, ..} <- ana_elab' iΓ eτ1 iτ
        AER {term = iδ2, ..} <- ana_elab' iΓ eτ2 iτ
        (tequiv' iΓ iδ1 iδ2 iτ) &>> return ())

tequiv' :: Ctx -> Term -> Term -> Typ -> Bool
tequiv' iΓ δ1 δ2 τ = (term_normal iΓ δ1 τ) ≡ (term_normal iΓ δ2 τ)

kequiv' :: Ctx -> Typ -> Typ -> Bool
kequiv' iΓ τ1 τ2 =
  case (type_normal iΓ τ1, type_normal iΓ τ2) of
    (S ωτ1 ωδ1, S ωτ2 ωδ2) -> (kequiv' iΓ ωτ1 ωτ2) && (tequiv' iΓ ωδ1 ωδ2 ωτ1)
    (τ@(Π t _ _), τ'@(Π t' _ _)) ->
      let t'' = fresh2 t t'
       in case (αRename t'' t τ, αRename t'' t' τ') of
            (Π _ ωτ1 ωτ2, Π _ ωτ3 ωτ4) ->
              (kequiv' iΓ ωτ1 ωτ3) && (kequiv' (iΓ ⌢ (t'', ωτ1)) ωτ2 ωτ4)
            _ -> error "bad bad\n"
    (ωτ1, ωτ2) -> ωτ1 ≡ ωτ2

csk' :: Ctx -> Typ -> Typ -> Bool
csk' iΓ τ1 τ2 =
  case (type_normal iΓ τ1, type_normal iΓ τ2) of
    (KHole, _) -> True
    (_, KHole) -> True
    (S Type _, Type) -> True
    (Π t ωτ1 ωτ2, Π t' ωτ3 ωτ4) ->
      let t'' = fresh2 t t'
       in (csk' iΓ ωτ3 ωτ1) && csk' (iΓ ⌢ (t'', ωτ3)) ωτ2 ωτ4
    (ωδ1, ωδ2) -> kequiv' iΓ ωδ1 ωδ2

(≲) = ((.) flip) . flip $ csk'

data MPKResult =
  MPKR
    -- for use with record wildcards
    { tπ :: TID
    , iτIn :: Typ
    , iτOut :: Typ
    }
  deriving (Show)

mpk' :: Ctx -> Typ -> Maybe MPKResult
mpk' _ KHole = return $ MPKR freshfresh KHole KHole
mpk' iΓ τ =
  case type_normal iΓ τ of
    ωτ@(S KHole τ') ->
      let t = freshfresh
       in return $ MPKR t ωτ (S KHole (TAp τ' (TVar t)))
    Π t ωτ1 ωτ2 -> return $ MPKR t ωτ1 ωτ2
    _ -> Nothing
mpk' _ _ = Nothing

-- really should be ⊳Π, but Π is a letter...
(⊳→) = flip mpk'
