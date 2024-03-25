{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Plutarch.Utils (
    pinsert,
    pgetTrieId,
    pcountOfUniqueTokens,
    psingletonOfCS,
    pheadSingleton,
    passert,
    premoveElement,
    ptryLookupValue,
    dataListReplace,
    toHex,
    pcompareBS,
) where

import Data.Text qualified as T

import Plutarch.Api.V1.Value (KeyGuarantees)
import Plutarch.Api.V2 (AmountGuarantees, PCurrencySymbol, PMap (PMap), PTokenName, PValue (..))
import Plutarch.Monadic qualified as P
import Plutarch.Num ((#+), (#-))
import Plutarch.Prelude

pgetTrieId ::
    forall (anyOrder :: KeyGuarantees) (anyAmount :: AmountGuarantees) (s :: S).
    Term s (PValue anyOrder anyAmount :--> PCurrencySymbol :--> PByteString)
pgetTrieId = phoistAcyclic $ plam $ \val cs -> P.do
    passert "Too many assets" (pcountOfUniqueTokens # val #== 2)
    let x = psingletonOfCS # pdata cs # val
    PPair tn _amount <- pmatch x
    pto tn

pcompareBS :: Term s (PByteString :--> PByteString :--> PBool)
pcompareBS = plam $ \x y -> x #<= y

-- | Insert an element into a sorted list based on a comparison function.
pinsert :: forall (a :: PType) (s :: S). (PLift a) => Term s ((a :--> a :--> PBool) :--> a :--> PBuiltinList a :--> PBuiltinList a)
pinsert = pfix #$ plam $ \self comp x lst ->
    -- Define the insertion function that takes an element and a list,
    -- and inserts the element into the list based on the comparison function.
    let insertFn = plam $ \y ys -> pif (comp # x # y) (pcons # x # ys) (pcons # y # (self # comp # x # ys)) -- !!! here in the true case we just discard y, it is definitely wrong, please fix it !!!
    -- Use foldr to traverse the list and insert the element in the correct position.
     in pfoldr # insertFn # (pcons # x # pnil) # lst

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

premoveElement :: (PIsListLike list a) => Term s (PInteger :--> list a :--> list a)
premoveElement = phoistAcyclic $ plam $ (#) $ pfix #$ plam $ \self index l ->
    pif
        (index #== 0)
        (ptail # l)
        (pelimList (\x xs -> pcons # x # (self # (index #- 1) # xs)) l l)

ptryLookupValue ::
    forall
        (keys :: KeyGuarantees)
        (amounts :: AmountGuarantees)
        (s :: S).
    Term
        s
        ( PAsData PCurrencySymbol
            :--> PValue keys amounts
            :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
        )
ptryLookupValue = phoistAcyclic $
    plam $ \policyId val ->
        pmatch val $ \(PValue val') ->
            precList
                ( \self x xs ->
                    pif
                        (pfstBuiltin # x #== policyId)
                        ( pmatch (pfromData (psndBuiltin # x)) $ \(PMap tokens) ->
                            tokens
                        )
                        (self # xs)
                )
                (const perror)
                # pto val'

dataListReplace :: (PEq a, PIsData a) => Term s (a :--> a :--> PBuiltinList (PAsData a) :--> PBuiltinList (PAsData a))
dataListReplace = phoistAcyclic $ plam $ (#) $ pfix #$ plam $ \self original new l ->
    pelimList
        ( \x xs ->
            pif
                (pfromData x #== original)
                (pcons # pdata new # (self # original # new # xs))
                (pcons # x # (self # original # new # xs))
        )
        l
        l

-- | Convert PByteString to hexadecimal representation.
toHex :: Term s (PByteString :--> PByteString)
toHex = phoistAcyclic $ plam $ \bytes ->
    encodeBase16 # bytes # ((plengthBS # bytes) - 1) # pconstant ""

encodeBase16 :: Term s (PByteString :--> PInteger :--> PByteString :--> PByteString)
encodeBase16 = phoistAcyclic $ pfix #$ plam $ \self bytes ix builder ->
    pif
        (ix #< 0)
        builder
        ( P.do
            byte <- plet $ pindexBS # bytes # ix
            msb <- plet $ pdiv # byte # 16
            isb <- plet $ pmod # byte # 16
            let fst = msb #+ (pif (msb #< 10) 48 87)
                snd = isb #+ (pif (isb #< 10) 48 87)
                consd = pconsBS # fst #$ pconsBS # snd # builder
            self # bytes # (ix #- 1) # consd
        )
