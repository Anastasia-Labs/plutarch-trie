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
import Plutarch.Test.Precompiled (Expectation (Success), testEvalCase, tryFromPTerm)
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
            , withdrawal stakeCred 0
            ]

unitTest :: TestTree
unitTest = tryFromPTerm "Trie Unit Test" validator $ do
    testEvalCase
        "Pass"
        Success
        [ PlutusTx.toData genesisRedeemer
        , PlutusTx.toData createTrieContext
        ]
