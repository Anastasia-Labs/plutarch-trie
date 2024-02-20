{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Types (
    TrieAction (..),
    TrieDatum (..),
    PTrieAction (..),
    PTrieDatum (..),
) where

import Plutarch.DataRepr (
    DerivePConstantViaData (DerivePConstantViaData),
 )
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (PLifted))
import Plutarch.Prelude
import PlutusLedgerApi.V2 (
    BuiltinByteString,
    TxOutRef,
 )
import PlutusTx qualified

import Plutarch.Api.V2 (
    PTxOutRef,
 )

data TrieAction
    = Genesis TxOutRef Integer
    | Between Integer
    | Onto Integer
    deriving stock (Generic, Eq, Show)

PlutusTx.makeIsDataIndexed
    ''TrieAction
    [ ('Genesis, 0)
    , ('Between, 1)
    , ('Onto, 2)
    ]

data PTrieAction (s :: S)
    = PGenesis (Term s (PDataRecord '["inp" ':= PTxOutRef, "oidx" ':= PInteger]))
    | PBetween (Term s (PDataRecord '["oidx" ':= PInteger]))
    | POnto (Term s (PDataRecord '["oidx" ':= PInteger]))
    deriving stock (Generic)
    deriving anyclass (PlutusType, PIsData, PEq)

instance DerivePlutusType PTrieAction where type DPTStrat _ = PlutusTypeData

deriving anyclass instance
    PTryFrom PData (PAsData PTrieAction)

instance PUnsafeLiftDecl PTrieAction where
    type PLifted PTrieAction = TrieAction

deriving via
    (DerivePConstantViaData TrieAction PTrieAction)
    instance
        PConstantDecl TrieAction

data TrieDatum
    = TrieDatum BuiltinByteString [BuiltinByteString]
    | TrieOriginState BuiltinByteString
    deriving stock (Generic, Eq, Show)

PlutusTx.makeIsDataIndexed
    ''TrieDatum
    [ ('TrieDatum, 0)
    , ('TrieOriginState, 1)
    ]

data PTrieDatum (s :: S)
    = PTrieDatum (Term s (PDataRecord '["key" ':= PByteString, "children" ':= PBuiltinList PByteString]))
    | PTrieOriginState (Term s (PDataRecord '["required_withdrawal" ':= PByteString]))
    deriving stock (Generic)
    deriving anyclass (PlutusType, PIsData, PEq)

instance DerivePlutusType PTrieDatum where type DPTStrat _ = PlutusTypeData

-- deriving anyclass instance
--   PTryFrom PData (PAsData PTrieDatum)

instance PUnsafeLiftDecl PTrieDatum where
    type PLifted PTrieDatum = TrieDatum

deriving via
    (DerivePConstantViaData TrieDatum PTrieDatum)
    instance
        PConstantDecl TrieDatum
