{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Cardano.Binary qualified as CBOR
import Data.Aeson (KeyValue ((.=)), object)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Bifunctor (
    first,
 )
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as LBS
import Data.Default (def)
import Data.Text (
    Text,
    pack,
 )
import Data.Text.Encoding qualified as Text
import Plutarch (
    Config (Config),
    TracingMode (DoTracing, DoTracingAndBinds, NoTracing),
    compile,
 )
import Plutarch.Evaluate (
    evalScript,
 )
import Plutarch.Multivalidator (validator)
import Plutarch.Prelude
import Plutarch.Script (Script, serialiseScript)
import PlutusLedgerApi.V2 (
    Data,
    ExBudget,
 )
import Ply.Plutarch (
    writeTypedScript,
 )
import System.IO
import "liqwid-plutarch-extra" Plutarch.Extra.Script (
    applyArguments,
 )

encodeSerialiseCBOR :: Script -> Text
encodeSerialiseCBOR = Text.decodeUtf8 . Base16.encode . CBOR.serialize' . serialiseScript

evalT :: Config -> ClosedTerm a -> Either Text (Script, ExBudget, [Text])
evalT cfg x = evalWithArgsT cfg x []

evalWithArgsT :: Config -> ClosedTerm a -> [Data] -> Either Text (Script, ExBudget, [Text])
evalWithArgsT cfg x args = do
    cmp <- compile cfg x
    let (escr, budg, trc) = evalScript $ applyArguments cmp args
    scr <- first (pack . show) escr
    pure (scr, budg, trc)

writePlutusScript :: Config -> String -> FilePath -> ClosedTerm a -> IO ()
writePlutusScript cfg title filepath term = do
    putStrLn $ "Writing script to file " <> filepath <> "..."
    case evalT cfg term of
        Left e -> putStrLn (show e)
        Right (script, _, _) -> do
            let
                scriptType = "PlutusScriptV2" :: String
                plutusJson = object ["type" .= scriptType, "description" .= title, "cborHex" .= encodeSerialiseCBOR script]
                content = encodePretty plutusJson
            LBS.writeFile filepath content
    putStrLn "...done"

writePlutusScriptTraceBind :: String -> FilePath -> ClosedTerm a -> IO ()
writePlutusScriptTraceBind = writePlutusScript (Config DoTracingAndBinds)

writePlutusScriptTrace :: String -> FilePath -> ClosedTerm a -> IO ()
writePlutusScriptTrace = writePlutusScript (Config DoTracing)

writePlutusScriptNoTrace :: String -> FilePath -> ClosedTerm a -> IO ()
writePlutusScriptNoTrace = writePlutusScript (Config NoTracing)

main :: IO ()
main = do
    putStrLn "Writing Plutus Scripts to files"
    writePlutusScriptNoTrace "Multivalidator" "./compiled/multivalidator.json" validator
