module Spec.UtilsSpec (utilsTest) where

import Plutarch.Utils(pinsert)
import Test.Tasty (TestTree, testGroup)
import Plutarch.Prelude
import Plutarch.List (PList)
import Test.Tasty.HUnit
import Test.Hspec

-- A simple comparison function for integers that can be used with pinsert
pcompare :: Term s (PInteger :--> PInteger :--> PBool)
pcompare = plam $ \x y -> x #<= y

utilsTest :: TestTree
utilsTest = testGroup "pinsert tests"
  [ testpinsert "Insert into empty list" pnil 3 (pcons # pconstant 3 # pnil)
  , testpinsert "Insert into a single-element list at the beginning" (pcons # pconstant 5 # pnil) 3 (pcons # pconstant 3 # pcons # pconstant 5 # pnil)
  , testpinsert "Insert into a single-element list at the end" (pcons # pconstant 2 # pnil) 3 (pcons # pconstant 2 # pcons # pconstant 3 # pnil)
  , testpinsert "Insert into a multi-element list at the beginning" (pcons # pconstant 4 # pcons # pconstant 5 # pcons # pconstant 6 # pnil) 2 (pcons # pconstant 2 # pcons # pconstant 4 # pcons # pconstant 5 # pcons # pconstant 6 # pnil)
  , testpinsert "Insert into a multi-element list in the middle" (pcons # pconstant 2 # pcons # pconstant 4 # pcons # pconstant 6 # pnil) 5 (pcons # pconstant 2 # pcons # pconstant 4 # pcons # pconstant 5 # pcons # pconstant 6 # pnil)
  , testpinsert "Insert into a multi-element list at the end" (pcons # pconstant 2 # pcons # pconstant 4 # pcons # pconstant 6 # pnil) 7 (pcons # pconstant 2 # pcons # pconstant 4 # pcons # pconstant 6 # pcons # pconstant 7 # pnil)
  ]

-- Helper function to create a test case for pinsert
testpinsert :: String -> Term s (PList PInteger) -> Integer -> Term s (PList PInteger) -> TestTree
testpinsert name lst x expected = testCase name $ do
  let result = pinsert # pconstant x # pcompare # lst
  let isEqual = plistEquals # expected # result
  passert isEqual

-- passert :: ClosedTerm a -> Expectation
-- passert p = p pshouldBe (pcon PTrue)
