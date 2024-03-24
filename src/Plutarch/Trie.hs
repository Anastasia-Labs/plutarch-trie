-- {-# LANGUAGE AllowAmbiguousTypes #-}
-- {-# LANGUAGE OverloadedRecordDot #-}
-- {-# LANGUAGE PolyKinds #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Plutarch.Trie (
    ptrieHandler,
)
where

-- TrieAction (..),
-- TrieDatum (..),

-- PTrieDatum (..)

import Plutarch.Api.V1.Address (
    PCredential (PScriptCredential),
    PStakingCredential,
 )
import Plutarch.Api.V1.Maybe (PMaybeData (..))
import Plutarch.Api.V1.Value (pnormalize)
import Plutarch.Api.V2 (PCurrencySymbol (..), POutputDatum (..), PStakingCredential (..), PTxInInfo)
import Plutarch.Api.V2.Contexts (PTxInfo)
import Plutarch.Builtin (pforgetData, pserialiseData)
import Plutarch.Crypto (pblake2b_256)
import Plutarch.Extra.ScriptContext (pfromPDatum)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Types (
    PTrieAction (..),
    PTrieDatum (..),
 )
import Plutarch.Utils (passert, pgetTrieId, pheadSingleton, premoveElement, ptryLookupValue)

ptrieHandler ::
    ClosedTerm
        ( PStakingCredential
            :--> PTrieAction
            :--> PTxInfo
            :--> PBool
        )
ptrieHandler = phoistAcyclic $
    plam $ \cred rdm txinfo' -> P.do
        PStakingHash ((pfield @"_0" #) -> ownCredential) <- pmatch cred
        PScriptCredential ((pfield @"_0" #) -> ownCurrencySymbolByteString) <- pmatch ownCredential
        let ownCurrencySymbol = (pcon . PCurrencySymbol . pfromData . pto) ownCurrencySymbolByteString
        txinfoF <- pletFields @'["inputs", "outputs", "mint"] txinfo'
        inputs <- plet $ pfromData txinfoF.inputs
        outputs <- plet $ pfromData txinfoF.outputs
        let ins = pfilter @PBuiltinList # plam (\inp -> hasCredential # cred # inp) # inputs
        pmatch rdm $ \case
            PGenesis info -> P.do
                infoF <- pletFields @'["inp", "oidx"] info
                let trieId = pblake2b_256 # (pserialiseData # pforgetData infoF.inp)
                    expectedOutputs = premoveElement # infoF.oidx # outputs
                    newOutput = pelemAt # 0 # expectedOutputs
                    osOutput = pelemAt # 1 # expectedOutputs
                newOutputF <- pletFields @'["address", "value", "datum"] newOutput
                osOutputF <- pletFields @'["address", "value", "datum"] osOutput
                POutputDatum newRawDatum' <- pmatch newOutputF.datum
                POutputDatum osRawDatum' <- pmatch osOutputF.datum
                let newRawDatum = pfromPDatum @PTrieDatum # (pfield @"outputDatum" # newRawDatum')
                    newValue = pfromData newOutputF.value
                    osRawDatum = pfromPDatum @PTrieDatum # (pfield @"outputDatum" # osRawDatum')
                    osValue = pfromData osOutputF.value
                    newId = pgetTrieId # newValue # ownCurrencySymbol
                    osId = pgetTrieId # osValue # ownCurrencySymbol
                PTrieDatum newDatum <- pmatch newRawDatum
                PTrieOriginState _ <- pmatch osRawDatum
                newDatumF <- pletFields @'["key", "children"] newDatum
                passert "Incorrect ids" (newId #== trieId #&& osId #== trieId)
                passert
                    "Must include the genesis input"
                    ( pany @PBuiltinList
                        # plam
                            ( \inp ->
                                let outRef = pfield @"outRef" # inp
                                 in pif (outRef #== infoF.inp) (pcon PTrue) (pcon PFalse)
                            )
                        # inputs
                    )
                passert "Must empty key" (pfromData newDatumF.key #== pconstant "")
                passert "Must no children" (pnull # pfromData newDatumF.children)
                let tkPairs = ptryLookupValue # pdata ownCurrencySymbol # (pnormalize # txinfoF.mint)
                    tkPair = pheadSingleton # tkPairs
                    numMinted = psndBuiltin # tkPair
                    tkMinted = pfstBuiltin # tkPair
                    mintChecks =
                        pfromData numMinted
                            #== 2
                            #&& trieId
                            #== pto (pfromData tkMinted)

                passert "Incorrect Minting" mintChecks
                pif
                    (ptraceIfFalse "Trie Handler f1" (pnull # ins))
                    (pcon PTrue)
                    (pcon PFalse)
            POnto _continuingDatumFinfo -> P.do
                let headInput = pelemAt # 0 # inputs
                    continuingOutput = pelemAt # 0 # outputs
                    newOutput = pelemAt # 1 # outputs
                headInputF <- pletFields @["address", "value", "datum"] $ pfield @"resolved" # headInput
                continuingOutputF <- pletFields @'["address", "value", "datum"] continuingOutput
                newOutputF <- pletFields @'["address", "value", "datum"] newOutput
                let trieId = pgetTrieId # headInputF.value # ownCurrencySymbol
                POutputDatum rawHeadDatum' <- pmatch headInputF.datum
                POutputDatum rawContinuingDatum' <- pmatch continuingOutputF.datum
                POutputDatum rawNewDatum' <- pmatch newOutputF.datum
                let rawHeadDatum = pfromPDatum @PTrieDatum # (pfield @"outputDatum" # rawHeadDatum')
                    rawContinuingDatum = pfromPDatum @PTrieDatum # (pfield @"outputDatum" # rawContinuingDatum')
                    rawNewDatum = pfromPDatum @PTrieDatum # (pfield @"outputDatum" # rawNewDatum')
                PTrieDatum headDatum <- pmatch rawHeadDatum
                PTrieDatum continuingDatum <- pmatch rawContinuingDatum
                PTrieDatum newDatum <- pmatch rawNewDatum
                headDatumF <- pletFields @'["key", "children"] headDatum
                continuingDatumF <- pletFields @'["key", "children"] continuingDatum
                newDatumF <- pletFields @'["key", "children"] newDatum
                let headKey = headDatumF.key
                    headKeyLength = plengthBS # headKey
                    contKey = continuingDatumF.key
                    newKey = newDatumF.key
                    newKeySuffix = psliceBS # headKeyLength # (plengthBS # newKey - headKeyLength) # newKey
                    newKeySuffixFirstChar = psliceBS # 0 # 1 # newKeySuffix
                    contId = pgetTrieId # continuingOutputF.value # ownCurrencySymbol
                    newId = pgetTrieId # newOutputF.value # ownCurrencySymbol
                    tkPairs = ptryLookupValue # pdata ownCurrencySymbol # (pnormalize # txinfoF.mint)
                    tkPair = pheadSingleton # tkPairs
                    numMinted = psndBuiltin # tkPair
                    tkMinted = pfstBuiltin # tkPair
                    mintChecks =
                        pfromData numMinted
                            #== 1
                            #&& trieId
                            #== pto (pfromData tkMinted)
                passert "Must cont key == head key" (contId #== trieId #&& newId #== trieId)
                passert "Incorrect ids" (contKey #== headKey)
                passert "Incorrect new key" (headKey #== (psliceBS # 0 # headKeyLength # newKey))
                passert "Must empty new key suffix" (newKeySuffix #== pconstant "")
                passert "Incorrect Minting" mintChecks
                -- passert
                --   "Must continuing UTxO has 1 single new child"
                --   (pinsert # headDatumF.children # newKeySuffix # (#<=)) #== continuingDatumF.children
                passert
                    "Must include the genesis input"
                    ( pnot
                        #$ pany @PBuiltinList
                        # plam
                            ( \child ->
                                pif ((psliceBS # 0 # 1 # pfromData child) #== newKeySuffixFirstChar) (pcon PTrue) (pcon PFalse)
                            )
                        # headDatumF.children
                    )
                pif
                    (ptraceIfFalse "Trie Handler f2" (pnull @PBuiltinList # newDatumF.children))
                    (pcon PTrue)
                    (pcon PFalse)
            PBetween _ -> pcon PFalse

hasCredential :: ClosedTerm (PStakingCredential :--> PTxInInfo :--> PBool)
hasCredential = phoistAcyclic $
    plam $ \cred info -> P.do
        let resolved = pfield @"resolved" # info
            addr = pfield @"address" # resolved
            -- addr = pfield @"credential" # txoutF // FIXME(@hadelive): it should be credential not staking credential
            sc = pfield @"stakingCredential" # addr
        pmatch sc $ \case
            PDJust ((pfield @"_0" #) -> c) -> pif (cred #== c) (pcon PTrue) (pcon PFalse)
            _ -> pcon PFalse
