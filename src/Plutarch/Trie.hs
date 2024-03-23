-- {-# LANGUAGE AllowAmbiguousTypes #-}
-- {-# LANGUAGE OverloadedRecordDot #-}
-- {-# LANGUAGE PolyKinds #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE UndecidableInstances #-}

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
import Plutarch.Api.V2 (POutputDatum (..), PTxInInfo)
import Plutarch.Api.V2.Contexts (PTxInfo)
import Plutarch.Builtin (pforgetData, pserialiseData)
import Plutarch.Crypto (pblake2b_256)
import Plutarch.Extra.ScriptContext (pfromPDatum, ptryFromInlineDatum)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Types (
    PTrieAction (..),
    PTrieDatum (..),
 )
import Plutarch.Utils (passert, pgetTrieId, premoveIndexElement)
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (pletC, pletFieldsC, pmatchC)

ptrieHandler ::
    ClosedTerm
        ( PStakingCredential
            :--> PTrieAction
            :--> PTxInfo
            :--> PBool
        )
ptrieHandler = phoistAcyclic $
    plam $ \cred rdm txinfo' -> P.do
        PScriptCredential ownInputScriptCredential <- cred
        ownCredHash <- plet (pfield @"_0" # ownInputScriptCredential)
        txinfoF <- pletFields @'["inputs", "outputs", "mint"] txinfo'
        inputs <- plet $ pfromData txinfoF.inputs
        outputs <- plet $ pfromData txinfoF.outputs
        let ins = pfilter @PBuiltinList # plam (\inp -> hasCredential # cred # inp) # inputs
        pmatch rdm $ \case
            PGenesis info -> P.do
                infoF <- pletFields @'["inp", "oidx"] info
                let trieId = pblake2b_256 # (pserialiseData # pforgetData infoF.inp)
                    expectedOutputs = premoveIndexElement #$ infoF.oidx #$ outputs
                    newOutput = pelemAt # 0 # expectedOutputs
                    osOutput = pelemAt # 1 # expectedOutputs
                newOutputF <- pletFields @'["address", "value", "datum"] newOutput
                osOutputF <- pletFields @'["address", "value", "datum"] osOutput
                POutputDatum newRawDatum' <- pmatch newOutputF.datum
                POutputDatum osRawDatum' <- pmatch osOutputF.datum
                let newRawDatum = pfromPDatum @PTrieDatum # (pfield @"outputDatum" # newRawDatum')
                    newValue = pfromData newOutputF.value
                    msOutputDatum = pfromPDatum @PTrieDatum # (pfield @"outputDatum" # osRawDatum')
                    osValue = pfromData osOutputF.value
                    newId = pgetTrieId # newValue # ownCredHash
                    osId = pgetTrieId # osValue # ownCredHash
                passert "Incorrect ids" (newId #== trieId #&& osId #== trieId)
                pif
                    (ptraceIfFalse "Trie Handler f1" (pnull # ins))
                    (pcon PTrue)
                    (pcon PFalse)
            PBetween _ -> pcon PFalse
            POnto _ -> pcon PFalse

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
