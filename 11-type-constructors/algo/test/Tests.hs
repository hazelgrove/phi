{-# LANGUAGE PartialTypeSignatures #-}

import Algo hiding (tequiv)
import qualified Algo (tequiv)
import Test.HUnit

main :: IO ()
main = runTestTTAndExit tests

tests :: Test
tests =
  TestList . concat $
  [ [ Nil |- tequiv Bse Bse Type ~?= True
    -- , Nil |- tequiv Bse Bse (S Type Bse) ~?= True
    -- , Nil |- tequiv Bse Bse (Π "t" Type (S Type (TVar "t"))) ~?= False
    ]
  , [ Nil |- canon' Bse ~?= Just Bse
    , Nil |- canon' (TVar "T") ~?= Nothing
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
  , [ Nil |- canon' Type ~?= Just Type
    , Nil |- canon' (S Type Bse) ~?= Just (S Type Bse)
    ]
  ]

-- in a somewhat increasing order of complexity
(|-) :: _
aΓ |- f = f aΓ

-- tequiv τ1 τ2 κ = \aΓ -> Algo.tequiv aΓ τ1 τ2 κ
-- since application precedence is so high (and can't be competed against)
tequiv :: _
tequiv = ((.) . (.) $ flip) . ((.) flip) . flip $ Algo.tequiv
