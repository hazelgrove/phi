module Tests (tests) where

import Distribution.TestSuite
import Algo

tests :: IO [Test]
tests = return
  [
    Test (TestInstance
        { run = return $ Finished Pass
        , name = "foo"
        , tags = []
        , options = []
        , setOption = \_ _ -> Left "bar"
        })
  ]
