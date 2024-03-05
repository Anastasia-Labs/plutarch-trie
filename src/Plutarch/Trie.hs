{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
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
    inputs <- plet $ info.inputs
    let inputs = pfilter # plam (\inp -> hasCredential # cred # inp) # inputs
    pmatch rdm $ \case 
      PGenesis info -> pcon PTrue
      PBetween idx -> pcon PFalse
      POnto idx -> pcon PFalse

hasCredential :: ClosedTerm (PStakingCredential :--> PTxInInfo :--> PBool)
hasCredential = phoistAcyclic $
  plam $ \cred info -> P.do
    infoF <- pletFields @'["resolved"] info
    txoutF <- pletFields @'["address"] info.resolved
    addr' <- pletFields @'["credential"] txoutF.address
    pif (cred #== addr'.credential) (pcon PTrue) (pcon PFalse)
