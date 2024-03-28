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
import Plutarch.Utils (dataListReplace, passert, pcompareBS, pgetTrieId, pheadSingleton, pinsert, premoveElement, ptryLookupValue, toHex)

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
            POnto info -> P.do
                infoF <- pletFields @'["oidx"] info
                let expectedOutputs = premoveElement # infoF.oidx # outputs
                    headInput = pelemAt # 0 # inputs
                    continuingOutput = pelemAt # 0 # expectedOutputs
                    newOutput = pelemAt # 1 # expectedOutputs
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
                let headKey = toHex # headDatumF.key
                    headKeyLength = plengthBS # headKey
                    contKey = toHex # continuingDatumF.key
                    newKey = toHex # newDatumF.key
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
                passert
                    "Must continuing UTxO has 1 single new child"
                    ( (pinsert # pcompareBS # newKeySuffix # (pmap @PBuiltinList # plam (\child -> pfromData child) # headDatumF.children))
                        #== (pmap @PBuiltinList # plam (\child -> toHex # pfromData child) # continuingDatumF.children)
                    )
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
            PBetween info -> P.do
                infoF <- pletFields @'["oidx"] info
                let expectedOutputs = premoveElement # infoF.oidx # outputs
                    parentInput = pelemAt # 0 # inputs
                    continuingOutput = pelemAt # 0 # expectedOutputs
                    newOutput = pelemAt # 1 # expectedOutputs
                parentInputF <- pletFields @["address", "value", "datum"] $ pfield @"resolved" # parentInput
                continuingOutputF <- pletFields @'["address", "value", "datum"] continuingOutput
                newOutputF <- pletFields @'["address", "value", "datum"] newOutput
                POutputDatum rawParentDatum' <- pmatch parentInputF.datum
                POutputDatum rawContinuingDatum' <- pmatch continuingOutputF.datum
                POutputDatum rawNewDatum' <- pmatch newOutputF.datum
                let rawParentDatum = pfromPDatum @PTrieDatum # (pfield @"outputDatum" # rawParentDatum')
                    rawContinuingDatum = pfromPDatum @PTrieDatum # (pfield @"outputDatum" # rawContinuingDatum')
                    rawNewDatum = pfromPDatum @PTrieDatum # (pfield @"outputDatum" # rawNewDatum')
                PTrieDatum parentDatum <- pmatch rawParentDatum
                PTrieDatum continuingDatum <- pmatch rawContinuingDatum
                PTrieDatum newDatum <- pmatch rawNewDatum
                parentDatumF <- pletFields @'["key", "children"] parentDatum
                continuingDatumF <- pletFields @'["key", "children"] continuingDatum
                newDatumF <- pletFields @'["key", "children"] newDatum
                let parentKey = toHex # parentDatumF.key
                    parentKeyLength = plengthBS # parentKey
                    contKey = toHex # continuingDatumF.key
                    newKey = toHex # newDatumF.key
                    newKeySuffix = psliceBS # parentKeyLength # (plengthBS # newKey - parentKeyLength) # newKey
                    newKeySuffixFirstChar = psliceBS # 0 # 1 # newKeySuffix
                    newKeySuffixLength = plengthBS # newKeySuffix
                    contId = pgetTrieId # continuingOutputF.value # ownCurrencySymbol
                PJust childKeySuffixData <-
                    pmatch $
                        pfind @PBuiltinList
                            # plam
                                ( \child ->
                                    pif (toHex # (psliceBS # 0 # 1 # pfromData child) #== newKeySuffixFirstChar) (pcon PTrue) (pcon PFalse)
                                )
                            # parentDatumF.children
                let childKeySuffix = toHex # pfromData childKeySuffixData
                    newChildKeySuffix =
                        psliceBS # newKeySuffixLength # (plengthBS # childKeySuffix - newKeySuffixLength) # childKeySuffix

                passert
                    "New key prefix child key"
                    ((psliceBS # 0 # newKeySuffixLength # childKeySuffix) #== newKeySuffix)
                passert
                    "Child key isn't exactly new key"
                    (pnot #$ newChildKeySuffix #== pconstant "")
                passert
                    "New key isn't exactly parent key"
                    (pnot #$ newKeySuffix #== pconstant "")
                passert
                    "Parent prefixes new key"
                    (parentKey #== (psliceBS # 0 # parentKeyLength # newKey))
                passert
                    "Parent key is unchanged"
                    (parentKey #== contKey)
                passert
                    "All values are in same trie id"
                    ( contId
                        #== (pgetTrieId # newOutputF.value # ownCurrencySymbol)
                        #&& contId
                        #== (pgetTrieId # parentInputF.value # ownCurrencySymbol)
                    )
                passert
                    "Parent children updated sensibly"
                    ( plistEquals
                        # (pmap @PBuiltinList # plam (\child -> pdata (toHex # pfromData child)) # continuingDatumF.children)
                        # (dataListReplace # childKeySuffix # newKeySuffix # (pmap @PBuiltinList # plam (\child -> pdata (toHex # pfromData child)) # parentDatumF.children))
                    )
                pif
                    (ptraceIfFalse "Trie Handler f3" ((pmap @PBuiltinList # plam (\child -> pdata (toHex # pfromData child)) # newDatumF.children) #== psingleton # pdata newChildKeySuffix))
                    (pcon PTrue)
                    (pcon PFalse)

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
