{-# LANGUAGE PartialTypeSignatures #-}

import Algo hiding (tequiv)
import qualified Algo (tequiv)
import Test.HUnit

main :: IO ()
main = runTestTTAndExit tests

tests :: Test
tests =
  TestList
    [ Nil |- tequiv Bse Bse Type ~?= True
    , Nil |- tequiv Bse Bse (S Type Bse) ~?= True
    , Nil |- tequiv Bse Bse (Π "t" Type (S Type (TVar "t"))) ~?= False
    ]

-- in a somewhat increasing order of complexity
(|-) :: _
aΓ |- f = f aΓ

-- tequiv τ1 τ2 κ = \aΓ -> Algo.tequiv aΓ τ1 τ2 κ
-- since application precedence is so high (and can't be competed against)
tequiv :: _
tequiv = ((.) . (.) $ flip) . ((.) flip) . flip $ Algo.tequiv
