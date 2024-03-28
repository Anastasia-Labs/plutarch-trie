module Main (main) where

import Spec.TrieSpec (unitTest)
import Spec.UtilsSpec (utilsTest)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
    defaultMain $
        testGroup
            "Trie Test"
            [ utilsTest
            , unitTest
            ]
