{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Trie (

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

ptrieHandler ::
  ClosedTerm
    ( PStakingCredential
        :--> PTrieAction
        :--> PTxInfo
        :--> PBool
    )
ptrieHandler = phoistAcyclic $
  plam $ \cred rdm txinfo' ->  P.do
    info <- pletFields @'["inputs", "outputs", "mint"] txinfo'
    inputs <- plet $ pfromData info.inputs
    let inputs = pfilter @PBuiltinList # (plam (\inp -> hasCredential # cred # inp)) # inputs
    pmatch rdm $ \case 
      PGenesis info -> pcon PTrue
      PBetween idx -> pcon PFalse
      POnto idx -> pcon PFalse

hasCredential :: ClosedTerm (PStakingCredential :--> PTxInInfo :--> PBool)
hasCredential = phoistAcyclic $
  plam $ \cred info -> P.do
    let resolved = pfield @"resolved" # info
        addr = pfield @"address" # resolved
        -- addr = pfield @"credential" # txoutF
        sc = pfield @"stakingCredential" # addr
    pmatch sc $ \case
      PDJust ((pfield @"_0" #) -> c) -> pif (cred #== c) (pcon PTrue) (pcon PFalse)
      _ -> pcon PFalse
