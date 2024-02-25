module Spec.UtilsSpec (utilsTest) where

import PlutusTx qualified

import Plutarch.Prelude
import Plutarch.Test.Precompiled (tryFromPTerm, (@>))
import Plutarch.Utils (pinsert)

import Test.Tasty (TestTree)

-- A simple comparison function for integers that can be used with pinsert
pcompare :: Term s (PAsData PInteger :--> PAsData PInteger :--> PBool)
pcompare = plam $ \x y -> pfromData x #<= pfromData y

-- utilsTest :: TestTree
-- utilsTest = testGroup "pinsert tests"
--   [ testpinsert "Insert into empty list" pnil 3 (pcons # pconstant 3 # pnil)
--   , testpinsert "Insert into a single-element list at the beginning" (pcons # pconstant 5 # pnil) 3 (pcons # pconstant 3 # pcons # pconstant 5 # pnil)
--   , testpinsert "Insert into a single-element list at the end" (pcons # pconstant 2 # pnil) 3 (pcons # pconstant 2 # pcons # pconstant 3 # pnil)
--   , testpinsert "Insert into a multi-element list at the beginning" (pcons # pconstant 4 # pcons # pconstant 5 # pcons # pconstant 6 # pnil) 2 (pcons # pconstant 2 # pcons # pconstant 4 # pcons # pconstant 5 # pcons # pconstant 6 # pnil)
--   , testpinsert "Insert into a multi-element list in the middle" (pcons # pconstant 2 # pcons # pconstant 4 # pcons # pconstant 6 # pnil) 5 (pcons # pconstant 2 # pcons # pconstant 4 # pcons # pconstant 5 # pcons # pconstant 6 # pnil)
--   , testpinsert "Insert into a multi-element list at the end" (pcons # pconstant 2 # pcons # pconstant 4 # pcons # pconstant 6 # pnil) 7 (pcons # pconstant 2 # pcons # pconstant 4 # pcons # pconstant 6 # pcons # pconstant 7 # pnil)
--   ]

-- Helper function to create a test case for pinsert
utilsTest :: TestTree
utilsTest = tryFromPTerm "pinsert tests" testpinsert $ do
  [ PlutusTx.toData (3 :: Integer), PlutusTx.toData ([] :: [Integer]), PlutusTx.toData ([3] :: [Integer]) ] @> "Insert into empty list"
  -- [ ] @> "Insert into a single-element list at the beginning" (pcons # pconstant 5 # pnil) 3 (pcons # pconstant 3 # pcons # pconstant 5 # pnil) ]
  -- [ ] @> "Insert into a single-element list at the end" (pcons # pconstant 2 # pnil) 3 (pcons # pconstant 2 # pcons # pconstant 3 # pnil) ]
  -- [ ] @> "Insert into a multi-element list at the beginning" (pcons # pconstant 4 # pcons # pconstant 5 # pcons # pconstant 6 # pnil) 2 (pcons # pconstant 2 # pcons # pconstant 4 # pcons # pconstant 5 # pcons # pconstant 6 # pnil) ]
  -- [ ] @> "Insert into a multi-element list in the middle" (pcons # pconstant 2 # pcons # pconstant 4 # pcons # pconstant 6 # pnil) 5 (pcons # pconstant 2 # pcons # pconstant 4 # pcons # pconstant 5 # pcons # pconstant 6 # pnil) ]
  -- [ ] @> "Insert into a multi-element list at the end" (pcons # pconstant 2 # pcons # pconstant 4 # pcons # pconstant 6 # pnil) 7 (pcons # pconstant 2 # pcons # pconstant 4 # pcons # pconstant 6 # pcons # pconstant 7 # pnil) ]
  -- [ PlutusTx.toData x, PlutusTx.toData lst ] @> "should be true"
 where
  testpinsert = plam $ \x l expected ->
    pif (pfromData expected #== pinsert # pcompare # x # pfromData l)
        (pcon PUnit)
        (perror)
  -- testCase name $ do
  -- let result = pinsert # pconstant x # pcompare # lst
  -- let isEqual = plistEquals # expected # result
  -- isEqual #@?= PTrue



-- passert :: ClosedTerm a -> Expectation
-- passert p = p pshouldBe (pcon PTrue)
