-- {-# LANGUAGE AllowAmbiguousTypes #-}
-- {-# LANGUAGE OverloadedRecordDot #-}
-- {-# LANGUAGE PolyKinds #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Plutarch.Trie (
    ptrieHandler,
)
where

import Plutarch.Api.V1.Address (
    PCredential (PScriptCredential),
    PStakingCredential,
 )
import Plutarch.Api.V1.Maybe (PMaybeData (..))
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
import Plutarch.Utils (dataListReplace, passert, pcompareBS, pinsert)
import qualified Plutarch.Utils as Utils

ptrieHandler ::
    ClosedTerm
        ( PStakingCredential
            :--> PTrieAction
            :--> PTxInfo
            :--> PBool
        )
ptrieHandler =
    plam $ \cred rdm txinfo' -> P.do
        PStakingHash      ((pfield @"_0" #) -> ownCredential)               <- pmatch cred
        PScriptCredential ((pfield @"_0" #) -> ownCurrencySymbolByteString) <- pmatch ownCredential
        ownCurrencySymbol <- plet $ (pcon . PCurrencySymbol . pfromData . pto) ownCurrencySymbolByteString
        txinfoF           <- pletFields @'["inputs", "outputs", "mint"] txinfo'
        inputs            <- plet $ pfromData txinfoF.inputs
        outputs           <- plet $ pfromData txinfoF.outputs
        let ins           =  pfilter @PBuiltinList # plam (\inp -> hasCredential # ownCredential # inp) # inputs
        toHex             <- plet Utils.toHex
        premoveElement    <- plet Utils.premoveElement
        ptryLookupValue   <- plet Utils.ptryLookupValue
        pgetTrieId        <- plet Utils.pgetTrieId
        pheadSingleton    <- plet Utils.pheadSingleton
        pnormalize        <- plet Utils.pnormalize
        pmatch rdm $ \case
            PGenesis info -> P.do
                infoF           <- pletFields @'["inp", "oidx"] info
                trieId          <- plet $ pblake2b_256 # (pserialiseData # pforgetData infoF.inp)
                expectedOutputs <- plet $ premoveElement # infoF.oidx # outputs
                let newOutput   =  pelemAt # 0 # expectedOutputs
                let osOutput    =  pelemAt # 1 # expectedOutputs
                newOutputF      <- pletFields @'["address", "value", "datum"] newOutput
                osOutputF       <- pletFields @'["address", "value", "datum"] osOutput
                POutputDatum newRawDatum' <- pmatch newOutputF.datum
                POutputDatum osRawDatum'  <- pmatch osOutputF.datum
                let newRawDatum = pfromPDatum @PTrieDatum # (pfield @"outputDatum" # newRawDatum')
                let newValue    = pfromData newOutputF.value
                let osRawDatum  = pfromPDatum @PTrieDatum # (pfield @"outputDatum" # osRawDatum')
                let osValue     = pfromData osOutputF.value
                let newId       = pgetTrieId # newValue # ownCurrencySymbol
                let osId        = pgetTrieId # osValue # ownCurrencySymbol
                PTrieDatum newDatum <- pmatch newRawDatum
                PTrieOriginState _  <- pmatch osRawDatum
                newDatumF <- pletFields @'["key", "children"] newDatum
                passert "Incorrect ids" (newId #== trieId #&& osId #== trieId)
                passert
                    "Must include the genesis input"
                    ( pany @PBuiltinList
                        # plam (\inp -> pfield @"outRef" # inp #== infoF.inp)
                        # inputs
                    )
                passert "Must empty key"   (pfromData newDatumF.key #== pconstant "")
                passert "Must no children" (pnull # pfromData newDatumF.children)
                tkPairs    <- plet $ ptryLookupValue # ownCurrencySymbol # (pnormalize # txinfoF.mint)
                tkPair     <- plet $ pheadSingleton # tkPairs
                numMinted  <- plet $ psndBuiltin # tkPair
                tkMinted   <- plet $ pfstBuiltin # tkPair
                mintChecks <- plet $ pfromData numMinted #== 2 #&& trieId #== pto (pfromData tkMinted)
                passert "Incorrect Minting" mintChecks
                pif
                    (ptraceIfFalse "Trie Handler f1" (pnull # ins))
                    (pcon PTrue)
                    (pcon PFalse)

            POnto info -> P.do
                infoF                <- pletFields @'["oidx"] info
                expectedOutputs      <- plet $ premoveElement # infoF.oidx # outputs
                let headInput        =  pelemAt # 0 # inputs
                let continuingOutput =  pelemAt # 0 # expectedOutputs
                let newOutput        =  pelemAt # 1 # expectedOutputs
                headInputF           <- pletFields @["address", "value", "datum"] $ pfield @"resolved" # headInput
                continuingOutputF    <- pletFields @'["address", "value", "datum"] continuingOutput
                newOutputF           <- pletFields @'["address", "value", "datum"] newOutput
                trieId               <- plet $ pgetTrieId # headInputF.value # ownCurrencySymbol
                POutputDatum rawHeadDatum'       <- pmatch headInputF.datum
                POutputDatum rawContinuingDatum' <- pmatch continuingOutputF.datum
                POutputDatum rawNewDatum'        <- pmatch newOutputF.datum
                let rawHeadDatum       =  pfromPDatum @PTrieDatum # (pfield @"outputDatum" # rawHeadDatum')
                let rawContinuingDatum =  pfromPDatum @PTrieDatum # (pfield @"outputDatum" # rawContinuingDatum')
                let rawNewDatum        =  pfromPDatum @PTrieDatum # (pfield @"outputDatum" # rawNewDatum')
                PTrieDatum headDatum       <- pmatch rawHeadDatum
                PTrieDatum continuingDatum <- pmatch rawContinuingDatum
                PTrieDatum newDatum        <- pmatch rawNewDatum
                headDatumF                 <- pletFields @'["key", "children"] headDatum
                continuingDatumF           <- pletFields @'["key", "children"] continuingDatum
                newDatumF                  <- pletFields @'["key", "children"] newDatum
                headKey                    <- plet $ toHex # headDatumF.key
                headKeyLength              <- plet $ plengthBS # headKey
                let contKey                =  toHex # continuingDatumF.key
                newKey                     <- plet $ toHex # newDatumF.key
                newKeySuffix               <- plet $ psliceBS # headKeyLength # (plengthBS # newKey - headKeyLength) # newKey
                let newKeySuffixFirstChar  =  psliceBS # 0 # 1 # newKeySuffix
                let contId                 =  pgetTrieId # continuingOutputF.value # ownCurrencySymbol
                let newId                  =  pgetTrieId # newOutputF.value # ownCurrencySymbol
                let tkPairs                =  ptryLookupValue # ownCurrencySymbol # (pnormalize # txinfoF.mint)
                tkPair                     <- plet $ pheadSingleton # tkPairs
                let numMinted              =  psndBuiltin # tkPair
                let tkMinted               =  pfstBuiltin # tkPair
                passert "Must cont key == head key" (contId #== trieId #&& newId #== trieId)
                -- passert "Incorrect ids"             (contKey #== headKey)
                -- passert "Incorrect new key"         (headKey #== (psliceBS # 0 # headKeyLength # newKey))
                -- passert "Must empty new key suffix" (newKeySuffix #== pconstant "")
                -- passert "Incorrect Minting"         (pfromData numMinted #== 1 #&& trieId #== pto (pfromData tkMinted))
                -- passert
                --     "Must continuing UTxO has 1 single new child"
                --     ( (pinsert # pcompareBS # newKeySuffix # (pmap @PBuiltinList # plam (\child -> pfromData child) # headDatumF.children))
                --         #== (pmap @PBuiltinList # plam (\child -> toHex # pfromData child) # continuingDatumF.children)
                --     )
                -- passert
                --     "Must include the genesis input"
                --     ( pnot
                --         #$ pany @PBuiltinList
                --         # plam
                --             ( \child ->
                --                 pif ((psliceBS # 0 # 1 # pfromData child) #== newKeySuffixFirstChar) (pcon PTrue) (pcon PFalse)
                --             )
                --         # headDatumF.children
                --     )
                -- pif
                --     (ptraceIfFalse "Trie Handler f2" (pnull @PBuiltinList # newDatumF.children))
                --     (pcon PTrue)
                --     (pcon PFalse)
                pcon PFalse

            PBetween info -> P.do
                -- infoF <- pletFields @'["oidx"] info
                -- expectedOutputs <- plet $ premoveElement # infoF.oidx # outputs
                -- parentInput <- plet $ pelemAt # 0 # inputs
                -- continuingOutput <- plet $ pelemAt # 0 # expectedOutputs
                -- newOutput <- plet $ pelemAt # 1 # expectedOutputs
                -- parentInputF <- pletFields @["address", "value", "datum"] $ pfield @"resolved" # parentInput
                -- continuingOutputF <- pletFields @'["address", "value", "datum"] continuingOutput
                -- newOutputF <- pletFields @'["address", "value", "datum"] newOutput
                -- POutputDatum rawParentDatum' <- pmatch parentInputF.datum
                -- POutputDatum rawContinuingDatum' <- pmatch continuingOutputF.datum
                -- POutputDatum rawNewDatum' <- pmatch newOutputF.datum
                -- rawParentDatum <- plet $ pfromPDatum @PTrieDatum # (pfield @"outputDatum" # rawParentDatum')
                -- rawContinuingDatum <- plet $ pfromPDatum @PTrieDatum # (pfield @"outputDatum" # rawContinuingDatum')
                -- rawNewDatum <- plet $ pfromPDatum @PTrieDatum # (pfield @"outputDatum" # rawNewDatum')
                -- PTrieDatum parentDatum <- pmatch rawParentDatum
                -- PTrieDatum continuingDatum <- pmatch rawContinuingDatum
                -- PTrieDatum newDatum <- pmatch rawNewDatum
                -- parentDatumF <- pletFields @'["key", "children"] parentDatum
                -- continuingDatumF <- pletFields @'["key", "children"] continuingDatum
                -- newDatumF <- pletFields @'["key", "children"] newDatum
                -- parentKey <- plet $ toHex # parentDatumF.key
                -- parentKeyLength <- plet $ plengthBS # parentKey
                -- contKey <- plet $ toHex # continuingDatumF.key
                -- newKey <- plet $ toHex # newDatumF.key
                -- newKeySuffix <- plet $ psliceBS # parentKeyLength # (plengthBS # newKey - parentKeyLength) # newKey
                -- newKeySuffixFirstChar <- plet $ psliceBS # 0 # 1 # newKeySuffix
                -- newKeySuffixLength <- plet $ plengthBS # newKeySuffix
                -- contId <- plet $ pgetTrieId # continuingOutputF.value # ownCurrencySymbol
                -- PJust childKeySuffixData <-
                --     pmatch $
                --         pfind @PBuiltinList
                --             # plam
                --                 ( \child ->
                --                     pif (toHex # (psliceBS # 0 # 1 # pfromData child) #== newKeySuffixFirstChar) (pcon PTrue) (pcon PFalse)
                --                 )
                --             # parentDatumF.children
                -- childKeySuffix <- plet $ toHex # pfromData childKeySuffixData
                -- newChildKeySuffix <- plet $
                --         psliceBS # newKeySuffixLength # (plengthBS # childKeySuffix - newKeySuffixLength) # childKeySuffix

                -- passert
                --     "New key prefix child key"
                --     ((psliceBS # 0 # newKeySuffixLength # childKeySuffix) #== newKeySuffix)
                -- passert
                --     "Child key isn't exactly new key"
                --     (pnot #$ newChildKeySuffix #== pconstant "")
                -- passert
                --     "New key isn't exactly parent key"
                --     (pnot #$ newKeySuffix #== pconstant "")
                -- passert
                --     "Parent prefixes new key"
                --     (parentKey #== (psliceBS # 0 # parentKeyLength # newKey))
                -- passert
                --     "Parent key is unchanged"
                --     (parentKey #== contKey)
                -- passert
                --     "All values are in same trie id"
                --     ( contId
                --         #== (pgetTrieId # newOutputF.value # ownCurrencySymbol)
                --         #&& contId
                --         #== (pgetTrieId # parentInputF.value # ownCurrencySymbol)
                --     )
                -- passert
                --     "Parent children updated sensibly"
                --     ( plistEquals
                --         # (pmap @PBuiltinList # plam (\child -> pdata (toHex # pfromData child)) # continuingDatumF.children)
                --         # (dataListReplace # childKeySuffix # newKeySuffix # (pmap @PBuiltinList # plam (\child -> pdata (toHex # pfromData child)) # parentDatumF.children))
                --     )
                -- pif
                --     (ptraceIfFalse "Trie Handler f3" ((pmap @PBuiltinList # plam (\child -> pdata (toHex # pfromData child)) # newDatumF.children) #== psingleton # pdata newChildKeySuffix))
                --     (pcon PTrue)
                --     (pcon PFalse)
                pcon PFalse

hasCredential :: ClosedTerm (PCredential :--> PTxInInfo :--> PBool)
hasCredential =
    plam $ \cred info -> P.do
        let resolved = pfield @"resolved" # info
            addr = pfield @"address" # resolved
            sc = pfield @"credential" # addr
        cred #== pfromData sc
