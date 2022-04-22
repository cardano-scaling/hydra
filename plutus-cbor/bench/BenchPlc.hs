-- | Criterion benchmark of serializing some Plutus values to ByteString using
-- this library, but also `cborg` as a reference.
module Main where

import           Hydra.Prelude              hiding ((<>))
import           TxGen

import           Codec.Serialise            (serialise)
import           Criterion.Main             (bench, bgroup, defaultMain, whnf)
import           Plutus.Codec.CBOR.Encoding (Encoding, encodeByteString,
                                             encodeInteger, encodeListLen,
                                             encodeMap, encodeMaybe,
                                             encodingToBuiltinByteString)
import           Plutus.V1.Ledger.Api       (Address (..), BuiltinByteString,
                                             Credential (PubKeyCredential, ScriptCredential),
                                             CurrencySymbol (CurrencySymbol),
                                             DatumHash (DatumHash),
                                             PubKeyHash (PubKeyHash),
                                             TokenName (TokenName),
                                             TxOut (TxOut),
                                             ValidatorHash (ValidatorHash),
                                             Value (getValue), toBuiltin,
                                             toData)
import           PlutusTx.Semigroup         ((<>))

main :: IO ()
main = do
  defaultMain
    [ bgroup
        "TxOut"
        [ bgroup
            "ada only"
            [ bench "plutus-cbor" $ whnf plutusSerialize txOutAdaOnly
            , bench "cborg" $ whnf cborgSerialize txOutAdaOnly
            ]
        , bgroup
            "20 assets"
            [ bench "plutus-cbor" $ whnf plutusSerialize txOut20Assets
            , bench "cborg" $ whnf cborgSerialize txOut20Assets
            ]
        , bgroup -- roughly current maxValSize=5000 on mainchain
            "80 assets"
            [ bench "plutus-cbor" $ whnf plutusSerialize txOut80Assets
            , bench "cborg" $ whnf cborgSerialize txOut80Assets
            ]
        , bgroup
            "100 assets"
            [ bench "plutus-cbor" $ whnf plutusSerialize txOut100Assets
            , bench "cborg" $ whnf cborgSerialize txOut100Assets
            ]
        ]
    ]
 where
  txOutAdaOnly = generateWith genAdaOnlyTxOut 42
  txOut20Assets = generateWith (genTxOut 20) 42
  txOut80Assets = generateWith (genTxOut 80) 42
  txOut100Assets = generateWith (genTxOut 100) 42

-- | Use the provided 'Serialise' instance for 'Data' from plutus.
cborgSerialize :: TxOut -> BuiltinByteString
cborgSerialize = toBuiltin . toStrict . serialise . toData

-- | Serialize a 'TxOut' to cbor using our on-chain encoder plutus-cbor, but run in Haskell.
plutusSerialize :: TxOut -> BuiltinByteString
plutusSerialize = encodingToBuiltinByteString . encodeTxOut

encodeTxOut :: TxOut -> Encoding
encodeTxOut (TxOut addr value datum) =
  encodeListLen 3
    <> encodeAddress addr
    <> encodeValue value
    <> encodeDatum datum
{-# INLINEABLE encodeTxOut #-}

-- NOTE 1: This is missing the header byte with network discrimination. For the
-- sake of getting an order of magnitude and moving forward, it is fine.
--
-- NOTE 2: This is ignoring any stake reference and assuming that all addresses
-- are plain script or payment addresses with no delegation whatsoever. Again,
-- see NOTE #1.
encodeAddress :: Address -> Encoding
encodeAddress Address{addressCredential} =
  encodeByteString (credentialToBytes addressCredential)
 where
  credentialToBytes = \case
    PubKeyCredential (PubKeyHash h)    -> h
    ScriptCredential (ValidatorHash h) -> h
{-# INLINEABLE encodeAddress #-}

encodeValue :: Value -> Encoding
encodeValue =
  encodeMap encodeCurrencySymbol (encodeMap encodeTokenName encodeInteger) . getValue
 where
  encodeCurrencySymbol (CurrencySymbol symbol) = encodeByteString symbol
  encodeTokenName (TokenName token) = encodeByteString token
{-# INLINEABLE encodeValue #-}

encodeDatum :: Maybe DatumHash -> Encoding
encodeDatum =
  encodeMaybe (\(DatumHash h) -> encodeByteString h)
{-# INLINEABLE encodeDatum #-}

-- mkClausifyCode :: StaticFormula -> Tx.CompiledCode [LRVars]
-- mkClausifyCode formula = $$(Tx.compile [|| runClausify ||])`Tx.applyCode` Tx.liftCode formula


{-

import PlutusTx qualified as Tx

import PlutusCore qualified as PLC
import PlutusCore.Default
import UntypedPlutusCore qualified as UPLC
import UntypedPlutusCore.Evaluation.Machine.Cek as Cek

import Criterion.Main
import Criterion.Types (Config (..))
import System.FilePath

{- | The Criterion configuration returned by `getConfig` will cause an HTML report
   to be generated.  If run via stack/cabal this will be written to the
   `plutus-benchmark` directory by default.  The -o option can be used to change
   this, but an absolute path will probably be required (eg, "-o=$PWD/report.html") . -}
getConfig :: Double -> IO Config
getConfig limit = do
  templateDir <- getDataFileName ("common" </> "templates")
  let templateFile = templateDir </> "with-iterations" <.> "tpl" -- Include number of iterations in HTML report
  pure $ defaultConfig {
                template = templateFile,
                reportFile = Just "report.html",
                timeLimit = limit
              }

type Term    = UPLC.Term PLC.NamedDeBruijn DefaultUni DefaultFun ()

{- | Given a DeBruijn-named term, give every variable the name "v".  If we later
   call unDeBruijn, that will rename the variables to things like "v123", where
   123 is the relevant de Bruijn index.-}
toNamedDeBruijnTerm
    :: UPLC.Term UPLC.DeBruijn DefaultUni DefaultFun ()
    -> UPLC.Term UPLC.NamedDeBruijn DefaultUni DefaultFun ()
toNamedDeBruijnTerm = UPLC.termMapNames UPLC.fakeNameDeBruijn

{- | Remove the textual names from a NamedDeBruijn term -}
toAnonDeBruijnTerm
    :: Term
    -> UPLC.Term UPLC.DeBruijn DefaultUni DefaultFun ()
toAnonDeBruijnTerm = UPLC.termMapNames (\(UPLC.NamedDeBruijn _ ix) -> UPLC.DeBruijn ix)

{- | Just extract the body of a program wrapped in a 'CompiledCodeIn'.  We use this a lot. -}
compiledCodeToTerm
    :: Tx.CompiledCodeIn DefaultUni DefaultFun a -> Term
compiledCodeToTerm (Tx.getPlc -> UPLC.Program _ _ body) = body

{- | Lift a Haskell value to a PLC term.  The constraints get a bit out of control
   if we try to do this over an arbitrary universe.-}
haskellValueToTerm
    :: Tx.Lift DefaultUni a => a -> Term
haskellValueToTerm = compiledCodeToTerm . Tx.liftCode


{- | Convert a de-Bruijn-named UPLC term to a Benchmark -}
benchTermCek :: Term -> Benchmarkable
benchTermCek term =
    nf (runTermCek) $! term -- Or whnf?

{- | Just run a term (used for tests etc.) -}
runTermCek :: Term -> EvaluationResult Term
runTermCek = unsafeExtractEvaluationResult . (\ (fstT,_,_) -> fstT) . runCekDeBruijn PLC.defaultCekParameters Cek.restrictingEnormous Cek.noEmitter

type Result = EvaluationResult Term

{- | Evaluate a PLC term and check that the result matches a given Haskell value
   (perhaps obtained by running the Haskell code that the term was compiled
   from).  We evaluate the lifted Haskell value as well, because lifting may
   produce reducible terms. The function is polymorphic in the comparison
   operator so that we can use it with both HUnit Assertions and QuickCheck
   Properties.  -}
cekResultMatchesHaskellValue :: Tx.Lift DefaultUni a => Term -> (Result -> Result -> b) -> a -> b
cekResultMatchesHaskellValue term matches value = (runTermCek term) `matches` (runTermCek $ haskellValueToTerm value)
-}
