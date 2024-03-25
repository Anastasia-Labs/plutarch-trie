module Plutarch.Multivalidator (multivalidator) where

import Plutarch.Api.V2 (PScriptContext)
import Plutarch.Builtin (pasConstr)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

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
