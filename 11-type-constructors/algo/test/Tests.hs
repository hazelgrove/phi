import Test.HUnit
import Algo

main = runTestTTAndExit tests

tests :: Test
tests = TestList
  [ tequiv Nil Bse Bse Type ~?= True
  , tequiv Nil Bse Bse (S Type Bse) ~?= True
  , tequiv Nil Bse Bse (Π "t" Type (S Type (TVar "t"))) ~?= False
  ]
