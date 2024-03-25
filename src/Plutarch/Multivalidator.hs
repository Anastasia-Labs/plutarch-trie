module Plutarch.Multivalidator (multivalidator, spend, main) where

import Plutarch.Api.V1 (PCredential (..))
import Plutarch.Api.V1.AssocMap qualified as AssocMap
import Plutarch.Api.V2 (PScriptContext, PScriptHash (..), PScriptPurpose (..), PValidator)
import Plutarch.Builtin (pasConstr)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import Plutarch.Utils (ptryOwnInput)
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (
    pletC,
    pletFieldsC,
    pmatchC,
 )

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
        (0 #== constrIndex)
        (punsafeCoerce $ mintingPolicy # redeemerOrDatum # (punsafeCoerce scriptContextOrRedeemer))
        (spendingValidator # redeemerOrDatum # scriptContextOrRedeemer)

spend :: Term s PValidator
spend = phoistAcyclic $
    plam $ \_ _ ctx -> unTermCont $ do
        ctxF <- pletFieldsC @'["txInfo", "purpose"] ctx
        PSpending ownRef' <- pmatchC ctxF.purpose
        ownRef <- pletC $ pfield @"_0" # ownRef'
        txInfoF <- pletFieldsC @'["inputs", "wdrl"] ctxF.txInfo
        ownInput <- pletC $ ptryOwnInput # txInfoF.inputs # ownRef
        ownInputF <- pletFieldsC @'["address"] ownInput
        PScriptCredential ownInputScriptCred <- pmatchC (pfield @"credential" # ownInputF.address)
        return $
            pmatch (AssocMap.plookup # ownInputScriptCred # txInfoF.wdrl) $ \case
                PJust _ -> (popaque $ pconstant ())
                PNothing -> perror

main :: Term s PValidator
main = phoistAcyclic $
    plam $ \_ redeemer ctx -> unTermCont $ do
        ctxF <- pletFieldsC @'["txInfo", "purpose"] ctx
        txInfoF <- pletFieldsC @'["wdrl"] ctxF.txInfo
        return $
            pmatch ctxF.purpose $ \case
                PMinting policy' ->
                    let ownPolicyId = pfield @"_0" # policy'
                     in pmatch (AssocMap.plookup # (PScriptCredential pdcons # pdata (pcon $ PScriptHash $ phexByteStr ownPolicyId) # pdnil) # txInfoF.wdrl) $ \case
                            PJust _ -> (popaque $ pconstant ())
                            PNothing -> perror
                PRewarding stakeCred' ->
                    let red :: Term _ PTrieAction
                        red = punsafeCoerce redeemer
                     in ptrieHandler stakeCred' red ctxF.txInfo

-- | TODO (@proxy)
-- validator = multivalidator spend main
