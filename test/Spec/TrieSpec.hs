module Spec.TrieSpec (
    unitTest,
) where

import Plutarch.Context (
    UTXO,
    address,
    buildMinting',
    input,
    mint,
    output,
    withInlineDatum,
    withMinting,
    withValue,
    withdrawal,
 )
import Plutarch.Multivalidator (validator)
import Plutarch.Test.Precompiled (Expectation (Failure, Success), testEvalCase, tryFromPTerm)
import Plutarch.Types (TrieAction (..), TrieDatum (..))
import PlutusLedgerApi.V2 (
    Address (..),
    Credential (..),
    CurrencySymbol (..),
    PubKeyHash (..),
    ScriptContext,
    StakingCredential (..),
    TokenName (..),
    TxOutRef (..),
    Value (..),
    singleton,
 )
import PlutusTx qualified
import Test.Tasty (TestTree)

genesisPKH :: PubKeyHash
genesisPKH = "b1f2f20a8781a3ba967d8c7b5068d21d799e809dcce22f651679d661"

genesisAddress :: Address
genesisAddress = Address (PubKeyCredential genesisPKH) Nothing

genesisUTXO :: UTXO
genesisUTXO =
    mconcat
        [ address genesisAddress
        , withValue (singleton "" "" 5_000_000)
        ]
stakeCred :: StakingCredential
stakeCred = StakingHash (PubKeyCredential "b055a795895b15d9af25acb752ac89c78524acfa387acb626c7e1bc8")

trieAddress :: Address
trieAddress =
    let cred = "b055a795895b15d9af25acb752ac89c78524acfa387acb626c7e1bc8"
     in Address (ScriptCredential cred) (Just stakeCred)

genesisRedeemer :: TrieAction
genesisRedeemer = Genesis (TxOutRef "b1f2f20a8781a3ba967d8c7b5068d21d799e809dcce22f651679d661" 1) 0

rewardingCred :: StakingCredential
rewardingCred = StakingHash (ScriptCredential "746fa3ba2daded6ab9ccc1e39d3835aa1dfcb9b5a54acc2ebe6b79a4")

trieTokenName :: TokenName
trieTokenName = "TestTN"

trieCurrencySymbol :: CurrencySymbol
trieCurrencySymbol = "746fa3ba2daded6ab9ccc1e39d3835aa1dfcb9b5a54acc2ebe6b79a4"

mintTrieValue :: Value
mintTrieValue = singleton trieCurrencySymbol trieTokenName 2

trieValue :: Value
trieValue = singleton trieCurrencySymbol trieTokenName 1

genesisDatum :: TrieDatum
genesisDatum = TrieDatum "" []

originState :: TrieDatum
originState = TrieOriginState ""

outputTrieValue :: Value
outputTrieValue = singleton "" "" 5_000_000 <> trieValue

outputTrieUTXO :: UTXO
outputTrieUTXO =
    mconcat
        [ address trieAddress
        , withValue outputTrieValue
        , withInlineDatum genesisDatum
        ]

outputOriginUTXO :: UTXO
outputOriginUTXO =
    mconcat
        [ address trieAddress
        , withValue outputTrieValue
        , withInlineDatum originState
        ]

createTrieContext :: ScriptContext
createTrieContext =
    buildMinting' $
        mconcat
            [ input genesisUTXO
            , output outputTrieUTXO
            , output outputOriginUTXO
            , mint mintTrieValue
            , withMinting trieCurrencySymbol
            , withdrawal rewardingCred 0
            ]

createTrieInvalidContext :: ScriptContext
createTrieInvalidContext =
    buildMinting' $
        mconcat
            [ input genesisUTXO
            , output outputTrieUTXO
            , output outputOriginUTXO
            , mint mintTrieValue
            , withMinting trieCurrencySymbol
            , withdrawal stakeCred 0
            ]

appendRedeemer :: TrieAction
appendRedeemer = Onto 0

newParentDatum :: TrieDatum
newParentDatum = TrieDatum "" ["68656c6c6f5f776f726c64"]

childDatum :: TrieDatum
childDatum = TrieDatum "68656c6c6f5f776f726c64" []

parentTrieUTXO :: UTXO
parentTrieUTXO =
    mconcat
        [ address trieAddress
        , withValue outputTrieValue
        , withInlineDatum newParentDatum
        ]

childTrieUTXO :: UTXO
childTrieUTXO =
    mconcat
        [ address trieAddress
        , withValue outputTrieValue
        , withInlineDatum childDatum
        ]

appendTrieContext :: ScriptContext
appendTrieContext =
    buildMinting' $
        mconcat
            [ input outputTrieUTXO
            , output parentTrieUTXO
            , output childTrieUTXO
            , mint mintTrieValue
            , withMinting trieCurrencySymbol
            , withdrawal rewardingCred 0
            ]

betweenRedeemer :: TrieAction
betweenRedeemer = Between 0

btNewParentDatum :: TrieDatum
btNewParentDatum = TrieDatum "" ["68656c6c6f"]

btChildDatum :: TrieDatum
btChildDatum = TrieDatum "68656c6c6f" ["5f776f726c64"]

newParentTrieUTXO :: UTXO
newParentTrieUTXO =
    mconcat
        [ address trieAddress
        , withValue outputTrieValue
        , withInlineDatum btNewParentDatum
        ]

btChildTrieUTXO :: UTXO
btChildTrieUTXO =
    mconcat
        [ address trieAddress
        , withValue outputTrieValue
        , withInlineDatum btChildDatum
        ]

betweenTrieContext :: ScriptContext
betweenTrieContext =
    buildMinting' $
        mconcat
            [ input parentTrieUTXO
            , output newParentTrieUTXO
            , output btChildTrieUTXO
            , mint mintTrieValue
            , withMinting trieCurrencySymbol
            , withdrawal rewardingCred 0
            ]

duplicateInsertionNewParentDatum :: TrieDatum
duplicateInsertionNewParentDatum = TrieDatum "" ["68656c6c6f5f776f726c64"]

duplicateInsertionChildDatum :: TrieDatum
duplicateInsertionChildDatum = TrieDatum "68656c6c6f5f776f726c64" []

duplicateInsertionNewParentTrieUTXO :: UTXO
duplicateInsertionNewParentTrieUTXO =
    mconcat
        [ address trieAddress
        , withValue outputTrieValue
        , withInlineDatum duplicateInsertionNewParentDatum
        ]

duplicateInsertionChildTrieUTXO :: UTXO
duplicateInsertionChildTrieUTXO =
    mconcat
        [ address trieAddress
        , withValue outputTrieValue
        , withInlineDatum duplicateInsertionChildDatum
        ]

duplicateInsertionBetweenTrieContext :: ScriptContext
duplicateInsertionBetweenTrieContext =
    buildMinting' $
        mconcat
            [ input parentTrieUTXO
            , output duplicateInsertionNewParentTrieUTXO
            , output duplicateInsertionChildTrieUTXO
            , mint mintTrieValue
            , withMinting trieCurrencySymbol
            , withdrawal rewardingCred 0
            ]

unitTest :: TestTree
unitTest = tryFromPTerm "Trie Unit Test" validator $ do
    testEvalCase
        "Pass - Create Trie"
        Success
        [ PlutusTx.toData genesisRedeemer
        , PlutusTx.toData createTrieContext
        ]
    testEvalCase
        "Fail - Create Trie"
        Failure
        [ PlutusTx.toData genesisRedeemer
        , PlutusTx.toData createTrieInvalidContext
        ]
    testEvalCase
        "Pass - Append Onto Trie"
        Success
        [ PlutusTx.toData appendRedeemer
        , PlutusTx.toData appendTrieContext
        ]
    testEvalCase
        "Pass - Append Between Trie"
        Success
        [ PlutusTx.toData betweenRedeemer
        , PlutusTx.toData betweenTrieContext
        ]
    testEvalCase
        "Fail - Duplicate Insertion Trie"
        Success
        [ PlutusTx.toData betweenRedeemer
        , PlutusTx.toData duplicateInsertionBetweenTrieContext
        ]
