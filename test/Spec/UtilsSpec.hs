module Spec.UtilsSpec (utilsTest) where

import PlutusTx qualified

import Plutarch.Prelude
import Plutarch.Test.Precompiled (tryFromPTerm, (@>))
import Plutarch.Utils (pinsert)

import Test.Tasty (TestTree)

-- A simple comparison function for integers that can be used with pinsert
pcompare :: Term s (PAsData PInteger :--> PAsData PInteger :--> PBool)
pcompare = plam $ \x y -> pfromData x #<= pfromData y

-- Helper function to create a test case for pinsert
utilsTest :: TestTree
utilsTest = tryFromPTerm "pinsert tests" testpinsert $ do
    [PlutusTx.toData (3 :: Integer), PlutusTx.toData ([] :: [Integer]), PlutusTx.toData ([3] :: [Integer])] @> "Insert into empty list"
    [PlutusTx.toData (3 :: Integer), PlutusTx.toData ([5] :: [Integer]), PlutusTx.toData ([3, 5] :: [Integer])] @> "Insert into a single-element list at the beginning"
    [PlutusTx.toData (3 :: Integer), PlutusTx.toData ([2] :: [Integer]), PlutusTx.toData ([2, 3] :: [Integer])] @> "Insert into a single-element list at the end"
    [PlutusTx.toData (2 :: Integer), PlutusTx.toData ([4, 5, 6] :: [Integer]), PlutusTx.toData ([2, 4, 5, 6] :: [Integer])] @> "Insert into a multi-element list at the beginning"
    [PlutusTx.toData (5 :: Integer), PlutusTx.toData ([2, 4, 6] :: [Integer]), PlutusTx.toData ([2, 4, 5, 6] :: [Integer])] @> "Insert into a multi-element list in the middle"
    [PlutusTx.toData (7 :: Integer), PlutusTx.toData ([2, 4, 6] :: [Integer]), PlutusTx.toData ([2, 4, 6, 7] :: [Integer])] @> "Insert into a multi-element list at the end"
  where
    testpinsert = plam $ \x l expected ->
        pif
            (pfromData expected #== pinsert # pcompare # x # pfromData l)
            (pcon PUnit)
            (perror)
