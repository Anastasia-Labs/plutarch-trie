{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Plutarch.Utils (
  pinsert
, pgetTrieId
, pcountOfUniqueTokens
, psingletonOfCS
, pheadSingleton
, passert
) where 

import Data.Text qualified as T

import Plutarch.Prelude
import Plutarch.Api.V2 (
  PCurrencySymbol,
  PMap (PMap),
  PTokenName,
  PValue (..),
 )
import Plutarch.Monadic qualified as P
import Plutarch.Api.V1.Value (KeyGuarantees)
import Plutarch.Api.V2 (AmountGuarantees)

pgetTrieId ::
  forall (anyOrder :: KeyGuarantees) (anyAmount :: AmountGuarantees) (s :: S).
  Term s (PValue anyOrder anyAmount :--> PCurrencySymbol :--> PByteString)
pgetTrieId = phoistAcyclic $ plam $ \val cs -> P.do
  passert "Too many assets" (pcountOfUniqueTokens # val #== 2)
  let x = psingletonOfCS # pdata cs # val
  PPair tn _amount <- pmatch x
  pto tn

{- | Insert an element into a sorted list based on a comparison function. -}
pinsert :: Term s (a :--> (a :--> a :--> PBool) :--> PList a :--> PList a)
pinsert = plam $ \x comp lst -> unTermCont $ do
  -- Define the insertion function that takes an element and a list,
  -- and inserts the element into the list based on the comparison function.
  let insertFn = plam $ \y ys -> pif (comp # x # y) (pcons # x # ys) (pcons # y # (pinsert # x # comp # ys))
  -- Use foldr to traverse the list and insert the element in the correct position.
  pure $ pfoldr # insertFn # (pcons # x # pnil) # lst

-- | Probably more effective than `plength . pflattenValue`
pcountOfUniqueTokens ::
  forall
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S).
  Term s (PValue keys amounts :--> PInteger)
pcountOfUniqueTokens = phoistAcyclic $
  plam $ \val ->
    let tokensLength = plam (\pair -> pmatch (pfromData $ psndBuiltin # pair) $ \(PMap tokens) -> plength # tokens)
     in pmatch val $ \(PValue val') ->
          pmatch val' $ \(PMap csPairs) -> pfoldl # plam (\acc x -> acc + (tokensLength # x)) # 0 # csPairs

psingletonOfCS ::
  forall
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S).
  Term
    s
    ( PAsData PCurrencySymbol
        :--> PValue keys amounts
        :--> PPair PTokenName PInteger
    )
psingletonOfCS = phoistAcyclic $
  plam $ \policyId val ->
    pmatch val $ \(PValue val') ->
      precList
        ( \self x xs ->
            pif
              (pfstBuiltin # x #== policyId)
              ( pmatch (pfromData (psndBuiltin # x)) $ \(PMap tokens) ->
                  let tkPair = pheadSingleton # tokens
                   in pcon (PPair (pfromData (pfstBuiltin # tkPair)) (pfromData (psndBuiltin # tkPair)))
              )
              (self # xs)
        )
        (const perror)
        # pto val'

-- Get the head of the list if the list contains exactly one element, otherwise error.
pheadSingleton :: (PListLike list, PElemConstraint list a) => Term s (list a :--> a)
pheadSingleton = phoistAcyclic $
  plam $ \xs ->
    pelimList
      (pelimList (\_ _ -> ptraceError "List contains more than one element."))
      (ptraceError "List is empty.")
      xs

{- | If the input is True then continue otherwise throw an error message.
   Short trace is a sequence of first letters of long trace words.
-}
passert ::
  forall (s :: S) (a :: PType).
  T.Text -> -- long trace
  Term s PBool ->
  Term s a ->
  Term s a
passert longErrorMsg b inp = pif b inp $ ptraceError (pconstant longErrorMsg)
