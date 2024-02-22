module Main(main) where

import Spec.UtilsSpec(utilsTest)
import Test.Tasty (defaultMain, testGroup)

main :: IO()
main = do
  defaultMain $
    testGroup
      "Unit Test Group"
      [ utilsTest
      ]
