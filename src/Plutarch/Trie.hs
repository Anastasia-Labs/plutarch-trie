{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Trie (
  ptrieHandler,
)
where 

import Plutarch.Prelude
import Plutarch.Types ( TrieAction (..),
  TrieDatum (..),
  PTrieAction (..),
  PTrieDatum (..))
import Plutarch.Api.V1.Address (
  PStakingCredential,
 )
import Plutarch.Api.V2 (PTxInInfo)
import Plutarch.Monadic qualified as P
import Plutarch.Api.V2.Contexts(PTxInfo)
import Plutarch.Api.V1.Maybe (PMaybeData(..))
import Plutarch.Builtin(pforgetData, pserialiseData)
import Plutarch.Api.V1 (
    PTxOutRef(..),
 )
import Plutarch.Crypto (pblake2b_256)

ptrieHandler ::
  ClosedTerm
    ( PStakingCredential
        :--> PTrieAction
        :--> PTxInfo
        :--> PBool
    )
ptrieHandler = phoistAcyclic $
  plam $ \cred rdm txinfo' ->  P.do
    txinfoF <- pletFields @'["inputs", "outputs", "mint"] txinfo'
    inputs <- plet $ pfromData txinfoF.inputs
    let ins = pfilter @PBuiltinList # plam (\inp -> hasCredential # cred # inp) # inputs
    pmatch rdm $ \case 
      PGenesis info -> P.do 
        infoF <- pletFields @'["inp", "oidx"] info
        let oidx = infoF.oidx
            trieId = pblake2b_256 # (pserialiseData # pforgetData infoF.inp)
        pif 
          (ptraceIfFalse "Trie Handler f1" (pnull # ins)) 
          (pcon PTrue) 
          (pcon PFalse)
      PBetween idx -> pcon PFalse
      POnto idx -> pcon PFalse

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
