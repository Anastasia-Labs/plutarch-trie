{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Plutarch.Utils (
    pinsert,
    pgetTrieId,
    pcountOfUniqueTokens,
    psingletonOfCS,
    pheadSingleton,
    passert,
    pfailIfFalse,
    premoveElement,
    ptryLookupValue,
    dataListReplace,
    toHex,
    pcompareBS,
    ptryOwnInput,
    pnormalize,
) where

import Data.Text qualified as T

import qualified Plutarch.Api.V1.AssocMap as AssocMap
import Plutarch.Api.V1.Value (KeyGuarantees)
import Plutarch.Api.V2 (AmountGuarantees, PCurrencySymbol, PMap (PMap), PTokenName, PTxInInfo, PTxOut, PTxOutRef, PValue (..))
import Plutarch.Monadic qualified as P
import Plutarch.Num ((#+), (#-))
import Plutarch.Prelude
import Plutarch.Api.V1 (AmountGuarantees(..))

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
pinsert = plam $ \cmp x ->
    precList
      (\self y ys -> ptrace "look at me fly 2!!!" $
        pif (cmp # x # y)
            (pcons # x # (pcons # y # ys))
            (pcons # y # (self # ys))
      )
      (const $ pcons # x # pnil)

-- | Probably more effective than `plength . pflattenValue`
pcountOfUniqueTokens ::
    forall
        (keys :: KeyGuarantees)
        (amounts :: AmountGuarantees)
        (s :: S).
    Term s (PValue keys amounts :--> PInteger)
pcountOfUniqueTokens =
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
psingletonOfCS =
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
pheadSingleton =
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

pfailIfFalse :: Term s (PString :--> PBool :--> a :--> a)
pfailIfFalse = plam $ \message b inp -> pif b inp (ptraceError message)

premoveElement :: (PIsListLike list a) => Term s (PInteger :--> list a :--> list a)
premoveElement = pfix #$ plam $ \self index l ->
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
        ( PCurrencySymbol
            :--> PValue keys amounts
            :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
        )
ptryLookupValue =
    plam $ \policyId val ->
        pmatch (AssocMap.plookup # policyId # pto val) $ \case
            PJust x  -> pto x
            PNothing -> perror

dataListReplace :: (PEq a, PIsData a) => Term s (a :--> a :--> PBuiltinList (PAsData a) :--> PBuiltinList (PAsData a))
dataListReplace = pfix #$ plam $ \self original new l ->
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
toHex = plam $ \bytes ->
        encodeBase16 # bytes # ((plengthBS # bytes) #- 1) # pconstant ""
    where
        encodeBase16 = pfix #$ plam $ \self bytes ix builder ->
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

ptryOwnInput :: (PIsListLike list PTxInInfo) => Term s (list PTxInInfo :--> PTxOutRef :--> PTxOut)
ptryOwnInput =
    plam $ \inputs ownRef ->
        precList (\self x xs -> pletFields @'["outRef", "resolved"] x $ \txInFields -> pif (ownRef #== txInFields.outRef) txInFields.resolved (self # xs)) (const perror) # inputs

pnormalize ::
  forall (any :: AmountGuarantees) (s :: S).
  Term s (PValue 'AssocMap.Sorted any :--> PValue 'AssocMap.Sorted 'NonZero)
pnormalize =
  plam $ \value ->
    pcon . PValue $
      AssocMap.pmapMaybe # plam normalizeTokenMap # pto value
  where
    normalizeTokenMap ::
      forall (s' :: S) (k :: S -> Type) (any1 :: AssocMap.KeyGuarantees).
      Term s' (AssocMap.PMap any1 k PInteger) ->
      Term s' (PMaybe (AssocMap.PMap any1 k PInteger))
    normalizeTokenMap tokenMap =
      plet (AssocMap.pmapMaybeData # plam nonZero # tokenMap) $ \normalMap ->
        pif
          (AssocMap.pnull # normalMap)
          (pcon PNothing)
          (pcon $ PJust normalMap)
    nonZero ::
      forall (s' :: S).
      Term s' (PAsData PInteger) ->
      Term s' (PMaybe (PAsData PInteger))
    nonZero intData =
      pif (intData #== zeroData) (pcon PNothing) (pcon $ PJust intData)
    zeroData :: forall (s' :: S). Term s' (PAsData PInteger)
    zeroData = pdata 0
