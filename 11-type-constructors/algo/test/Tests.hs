{-# LANGUAGE PartialTypeSignatures #-}

import Algo hiding (tequiv)
import qualified Algo (tequiv)
import Test.HUnit

main :: IO ()
main = runTestTTAndExit tests

tests :: Test
tests =
  TestList . concat $
  [ tequivTests
  , freshTests
  , fresh2Tests
  , canonTypTests
  , canonKndTests
  , αKndTests
  ]
  where
    tequivTests =
      [ Nil |- tequiv Bse Bse Type ~?= True
      -- , Nil |- tequiv Bse Bse (S Type Bse) ~?= True
      -- , Nil |- tequiv Bse Bse (Π "t" Type (S Type (TVar "t"))) ~?= False
      ]
    freshTests =
      [ fresh "t" ~?= "t1"
      , fresh "t1" ~?= "t2"
      , fresh "t9" ~?= "t10"
      , fresh "terrible_Name10" ~?= "terrible_Name11"
      ]
    fresh2Tests =
      [ fresh2 "t" "t" ~?= "tt1"
      , fresh2 "t1" "foo" ~?= "tfoo1"
      , fresh2 "t9" "foo10" ~?= "tfoo1"
      , fresh2 "terrible_Name10" "t" ~?= "terrible_Namet1"
      ]
    canonTypTests =
      [ Nil |- canon' Bse ~?= Just Bse
      , Nil |- canon' (TVar "T") ~?= Nothing
      , Nil |- canon' (TAp Bse Bse) ~?= Nothing
      , Nil ⌢ ("T", S Type Bse) |- canon' (TVar "T") ~?= Just Bse
      , Nil ⌢ ("T", S Type Bse) ⌢ ("V", S (S Type Bse) (TVar "T")) |-
        canon' (TVar "V") ~?=
        Just Bse
      , Nil ⌢
        ( "Pair"
        , S (Π "t" Type (S Type (TVar "t" :⊕ TVar "t")))
            (Tλ "t" Type (TVar "t" :⊕ TVar "t"))) |-
        canon' (TVar "Pair") ~?=
        (Just $ (Tλ "t" Type (TVar "t" :⊕ TVar "t")))
      , Nil ⌢
        ( "Pair"
        , S (Π "t" Type (S Type (TVar "t" :⊕ TVar "t")))
            (Tλ "t" Type (TVar "t" :⊕ TVar "t"))) |-
        canon' (TAp (TVar "Pair") Bse) ~?=
        (Just $ Bse :⊕ Bse)
      , Nil ⌢
        ( "Pair"
        , S (Π "t" Type (S Type (TVar "t" :⊕ TVar "t")))
            (Tλ "t" Type (TVar "t" :⊕ TVar "t"))) |-
        canon' (TAp (TVar "Pair") (TAp (TVar "Pair") (Bse))) ~?=
        (Just $ (Bse :⊕ Bse) :⊕ (Bse :⊕ Bse))
      , Nil ⌢
        ( "Pair"
        , S (Π "t" Type (S Type (TVar "t" :⊕ TVar "t")))
            (Tλ "t" Type (TVar "t" :⊕ TVar "t"))) |-
        canon' (TAp (TVar "Pair") (TVar "Pair")) ~?=
        Nothing
      ]
    canonKndTests =
      [ Nil |- canon' Type ~?= Just Type
      , Nil |- canon' (S Type Bse) ~?= Just (S Type Bse)
      , let τ = (Tλ "t" Type $ TVar "t")
         in let t = "t"
             in let t1 = fresh t
                 in Nil |- canon' (S (Π t Type Type) τ) ~?=
                    Just (Π t1 Type (S (αRename t1 t Type) $ TVar t1))
      ]
    αKndTests =
      [ Type ≡ Type ~?= True
      , Π "t" Type Type ≡ Π "t1" Type Type ~?= True
      , Π "t" Type (S Type $ TVar "t") ≡ Π "t1" Type (S Type $ TVar "t1") ~?=
        True
      , Π "t" Type (S Type $ TVar "t") ≡ Π "t1" Type (S Type $ TVar "t") ~?=
        False
      ]

-- in a somewhat increasing order of complexity
(|-) :: _
aΓ |- f = f aΓ

-- tequiv τ1 τ2 κ = \aΓ -> Algo.tequiv aΓ τ1 τ2 κ
-- since application precedence is so high (and can't be competed against)
tequiv :: _
tequiv = ((.) . (.) $ flip) . ((.) flip) . flip $ Algo.tequiv

class Canon a =>
      Canon' a
  where
  canon' :: a -> Ctx -> Maybe a
  canon' = flip canon

instance Canon' Typ

instance Canon' Knd
