import Test.HUnit
import Algo hiding (tequiv)
import qualified Algo (tequiv)

main = runTestTTAndExit tests

tests :: Test
tests = TestList
-- in a somewhat increasing order of complexity
  [ Nil |- tequiv Bse Bse Type ~?= True
  , Nil |- tequiv Bse Bse (S Type Bse) ~?= True
  , Nil |- tequiv Bse Bse (Π "t" Type (S Type (TVar "t"))) ~?= False
  ]

aΓ |- f = f aΓ

-- tequiv τ1 τ2 κ = \aΓ -> Algo.tequiv aΓ τ1 τ2 κ
tequiv = ((.).(.) $ flip).((.) flip).flip $ Algo.tequiv
