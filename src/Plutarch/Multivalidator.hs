{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Plutarch.Multivalidator (validator, multivalidator, spend, main) where

import Plutarch.Api.V1 (PCredential (..))
import Plutarch.Api.V1.AssocMap qualified as AssocMap
import Plutarch.Api.V2 (PScriptContext, PScriptHash (..), PScriptPurpose (..), PStakeValidator, PStakingCredential (..), PValidator)
import Plutarch.Builtin (pasConstr)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import Plutarch.Utils (ptryOwnInput)
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (
    pletC,
    pletFieldsC,
    pmatchC,
    ptraceC,
 )

import Plutarch.ByteString (pbyteStr)
import Plutarch.Trie (ptrieHandler)
import Plutarch.Types (PTrieAction (..))

{- | multivalidator:
In order for this script to be able to determine which script to delegate the call
the redeemer for the spending validator MUST be wrapped in a Constr 1. The easiest
way to achieve this is to define the redeemer for the spending validator as:
<code>
data MyRedeemer
  = NotUsed
  | Case1 ...
  | Case2 ...
</code>
-}
multivalidator :: Term s (PData :--> PScriptContext :--> POpaque) -> Term s (PData :--> PData :--> PScriptContext :--> POpaque) -> Term s (PData :--> PData :--> PScriptContext :--> POpaque)
multivalidator mintingPolicy spendingValidator = plam $ \redeemerOrDatum scriptContextOrRedeemer -> P.do
    let constrIndex = pfstBuiltin #$ pasConstr # scriptContextOrRedeemer
    pif
        (0 #< constrIndex)
        (spendingValidator # redeemerOrDatum # scriptContextOrRedeemer)
        (punsafeCoerce $ mintingPolicy # redeemerOrDatum # (punsafeCoerce scriptContextOrRedeemer))

spend :: Term s PValidator
spend =
    plam $ \_ _ ctx -> unTermCont $ do
        ptraceC "spend"
        ctxF <- pletFieldsC @'["txInfo", "purpose"] ctx
        PSpending ownRef' <- pmatchC ctxF.purpose
        ownRef <- pletC $ pfield @"_0" # ownRef'
        txInfoF <- pletFieldsC @'["inputs", "wdrl"] ctxF.txInfo
        ownInput <- pletC $ ptryOwnInput # txInfoF.inputs # ownRef
        ownInputF <- pletFieldsC @'["address"] ownInput
        let ownScriptCred = (pcon . PStakingHash) $ pdcons @"_0" # (pfield @"credential" # ownInputF.address) # pdnil
        return $
            pmatch (AssocMap.plookup # ownScriptCred # txInfoF.wdrl) $ \case
                PJust _ -> (popaque $ pconstant ())
                PNothing -> perror

main :: Term s PStakeValidator
main =
    plam $ \redeemer ctx -> unTermCont $ do
        ptraceC "main"
        ctxF <- pletFieldsC @'["txInfo", "purpose"] ctx
        txInfoF <- pletFieldsC @'["wdrl"] ctxF.txInfo
        ptraceC (pshow txInfoF.wdrl)
        return $
            pmatch ctxF.purpose $ \case
                PMinting ((pfield @"_0" #) -> policy) ->
                    let csByteString = (pfromData . pto) policy
                        ownCredential = pcon $ PStakingHash $ pdcons @"_0" # (pdata $ pcon $ PScriptCredential $ pdcons @"_0" # (pdata . pcon . PScriptHash) csByteString # pdnil) # pdnil
                     in pmatch (AssocMap.plookup # ownCredential # txInfoF.wdrl) $ \case
                            PJust _ -> (popaque $ pconstant ())
                            PNothing -> ptraceError (pshow ownCredential)
                PRewarding ((pfield @"_0" #) -> stakeCred) ->
                    let red = punsafeCoerce @_ @_ @PTrieAction redeemer
                     in pif
                            (ptrieHandler # stakeCred # red # ctxF.txInfo)
                            (popaque $ pconstant ())
                            perror
                _ -> perror

validator :: Term s PValidator
validator = multivalidator main spend
