module Main where

import Hydra.Prelude

import Criterion (bench, bgroup, nf, whnf)
import Criterion.Main (defaultMain)
import Data.Aeson (Value (String), object, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List qualified as List
import Data.Time (UTCTime (..), fromGregorian)
import Hydra.API.ClientInput (ClientInput (NewTx))
import Hydra.API.ServerOutput (
  ApiEncoding (..),
  Greetings (..),
  ServerOutput (..),
  ServerOutputConfig (..),
  TimedServerOutput (..),
  WithAddressedTx (..),
  WithUTxO (..),
  handleUtxoInclusionTyped,
  prepareServerOutput,
 )
import Hydra.API.WireFormat (decodeWire, encodeWire)
import Hydra.Cardano.Api (
  UTxO,
  serialiseToCBOR,
 )
import Hydra.Chain.ChainState (ChainSlot (ChainSlot))

-- Provides the 'IsChainState Tx' instance required by API server output codecs.
import Hydra.Chain.Direct.State ()
import Hydra.Ledger (Ledger (applyTransactions), ValidationError)
import Hydra.Ledger.Cardano (Tx, cardanoLedger)
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Crypto (aggregate, sign)
import Hydra.Tx.IsTx (IsTx (txId))
import Hydra.Tx.Snapshot (Snapshot (..))
import Test.Hydra.API.ServerOutput ()
import Test.Hydra.Ledger.Cardano (genFixedSizeSequenceOfSimplePaymentTransactions)
import Test.Hydra.Node.Fixture (defaultGlobals, defaultLedgerEnv)
import Test.Hydra.Tx.Fixture (aliceSk, bobSk, carolSk, testHeadId)
import Test.Hydra.Tx.Gen (genUTxOSized)
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (Gen, unGen)
import Test.QuickCheck.Random (mkQCGen)

main :: IO ()
main = do
  -- Use this env var to run benchmarks for more transactions:
  --
  -- > N_TXNS=1 cabal bench micro --benchmark-options '--json output.json +RTS -T'
  --
  -- You can then find the `peakMbAllocated` field in the resulting json file.
  -- (Sorry, criterion makes it very hard to find this with a simple jq
  -- command.)
  --
  nTxns <- fromMaybe 1 . (>>= readMaybe) <$> lookupEnv "N_TXNS"
  let (utxo, tx) = prepareTx nTxns
  let jsonNewTx = (Aeson.encode . NewTx) tx
      toNewTx :: ByteString -> Value
      toNewTx bs = object ["tag" .= ("NewTx" :: Text), "transaction" .= String (decodeUtf8 bs)]
      cborNewTx = (Aeson.encode . toNewTx . serialiseToCBOR) tx

  -- API server output messages as sent over the websocket. All values are
  -- generated with a fixed seed so payload sizes are reproducible across runs
  -- and comparable between wire formats.
  let txValid = mkTimed $ TxValid testHeadId (txId tx)
      snapshotsConfirmed = [(n, mkSnapshotConfirmed tx n) | n <- utxoSizes]
      greetings = mkGreetings
      greetingsJson = Aeson.encode greetings

  let cborWire :: (ToJSON a, ToCBOR a) => a -> LBS.ByteString
      cborWire = encodeWire CborEncoding

  putStrLn "Wire sizes (bytes):"
  putStrLn $ sizeRow "message" "JSON" "CBOR"
  putStrLn $ sizeRow "NewTx" (show $ LBS.length jsonNewTx) (show . LBS.length $ cborWire (NewTx tx))
  putStrLn $ sizeRow "NewTx transaction (raw ledger CBOR floor)" "-" (show . BS.length $ serialiseToCBOR tx)
  putStrLn $ sizeRow "TxValid" (show . LBS.length $ Aeson.encode txValid) (show . LBS.length $ cborWire txValid)
  forM_ snapshotsConfirmed $ \(n, timed) ->
    putStrLn $ sizeRow ("SnapshotConfirmed utxo=" <> show n) (show . LBS.length $ Aeson.encode timed) (show . LBS.length $ cborWire timed)
  putStrLn $ sizeRow "Greetings utxo=100" (show $ LBS.length greetingsJson) (show . LBS.length $ cborWire greetings)
  putStrLn ""

  defaultMain
    [ bgroup
        "Cardano Ledger"
        [ bench "Apply Tx" $ whnf benchApplyTxs (utxo, tx)
        , bench "Serialize NewTx (JSON)" $ nf (Aeson.encode . NewTx) tx
        , bench "Serialize NewTx (CBOR)" $ nf serialiseToCBOR tx
        , bench "Serialize NewTx (CBOR wire)" $ nf (encodeWire CborEncoding . NewTx) tx
        , bench "Deserialize NewTx (JSON)" $ whnf (Aeson.decode @(ClientInput Tx)) jsonNewTx
        , bench "Deserialize NewTx (CBOR-in-JSON)" $ whnf (Aeson.decode @(ClientInput Tx)) cborNewTx
        , bench "Deserialize NewTx (CBOR wire)" $ whnf decodeInputCbor (encodeWire CborEncoding (NewTx tx))
        ]
    , bgroup "API server output" $
        [ bench "Encode TxValid (JSON)" $ nf Aeson.encode txValid
        , bench "Encode TxValid (CBOR)" $ nf (encodeWire CborEncoding) txValid
        , bench "Decode TxValid (JSON)" $ whnf decodeTimed (Aeson.encode txValid)
        , bench "Decode TxValid (CBOR)" $ whnf decodeTimedCbor (encodeWire CborEncoding txValid)
        ]
          <> concatMap
            ( \(n, timed) ->
                let benchLabel = "SnapshotConfirmed utxo=" <> show n
                 in [ bench ("Encode " <> benchLabel <> " (JSON)") $ nf Aeson.encode timed
                    , bench ("Encode " <> benchLabel <> " (CBOR)") $ nf (encodeWire CborEncoding) timed
                    , bench ("Send " <> benchLabel <> " WithUTxO (JSON)") $ nf (prepareServerOutput withUTxOConfig) timed
                    , bench ("Send " <> benchLabel <> " WithoutUTxO (JSON)") $ nf (prepareServerOutput withoutUTxOConfig) timed
                    , bench ("Send " <> benchLabel <> " WithoutUTxO (CBOR)") $ nf (encodeWire CborEncoding . handleUtxoInclusionTyped withoutUTxOConfig) timed
                    , bench ("Decode " <> benchLabel <> " (JSON)") $ whnf decodeTimed (Aeson.encode timed)
                    , bench ("Decode " <> benchLabel <> " (CBOR)") $ whnf decodeTimedCbor (encodeWire CborEncoding timed)
                    ]
            )
            snapshotsConfirmed
          <> [ bench "Encode Greetings utxo=100 (JSON)" $ nf Aeson.encode greetings
             , bench "Encode Greetings utxo=100 (CBOR)" $ nf (encodeWire CborEncoding) greetings
             , bench "Decode Greetings utxo=100 (JSON)" $ whnf (Aeson.eitherDecode' @(Greetings Tx)) greetingsJson
             , bench "Decode Greetings utxo=100 (CBOR)" $ whnf decodeGreetingsCbor (encodeWire CborEncoding greetings)
             ]
    ]
 where
  decodeTimed = Aeson.eitherDecode' @(TimedServerOutput Tx)

  decodeTimedCbor :: LBS.ByteString -> Either String (TimedServerOutput Tx)
  decodeTimedCbor = decodeWire CborEncoding

  decodeGreetingsCbor :: LBS.ByteString -> Either String (Greetings Tx)
  decodeGreetingsCbor = decodeWire CborEncoding

  decodeInputCbor :: LBS.ByteString -> Either String (ClientInput Tx)
  decodeInputCbor = decodeWire CborEncoding

  withUTxOConfig = ServerOutputConfig{utxoInSnapshot = WithUTxO, addressInTx = WithoutAddressedTx, encoding = JsonEncoding}

  withoutUTxOConfig = ServerOutputConfig{utxoInSnapshot = WithoutUTxO, addressInTx = WithoutAddressedTx, encoding = JsonEncoding}

utxoSizes :: [Int]
utxoSizes = [1, 10, 100, 1000]

-- | Generate a value with a fixed seed so generated payloads (and hence wire
-- sizes) are reproducible across benchmark runs.
generateWith :: Gen a -> Int -> a
generateWith g seed = unGen g (mkQCGen seed) 30

prepareTx :: Int -> (UTxO, Tx)
prepareTx n =
  second List.head $ generateWith (genFixedSizeSequenceOfSimplePaymentTransactions n) 42

mkTimed :: ServerOutput Tx -> TimedServerOutput Tx
mkTimed output = TimedServerOutput{output, seq = 1, time = UTCTime (fromGregorian 2026 7 16) 0}

-- | A 'SnapshotConfirmed' server output with a snapshot over a UTxO set of
-- given size, signed by three parties.
mkSnapshotConfirmed :: Tx -> Int -> TimedServerOutput Tx
mkSnapshotConfirmed tx n =
  mkTimed $ SnapshotConfirmed testHeadId snapshot signatures
 where
  signatures = aggregate [sign sk snapshot | sk <- [aliceSk, bobSk, carolSk]]

  snapshot =
    Snapshot
      { headId = testHeadId
      , version = 1
      , number = 2
      , confirmed = [tx]
      , utxo = u
      , utxoToCommit = Nothing
      , depositTxId = Nothing
      , utxoToDecommit = Nothing
      , accumulator = Accumulator.buildFromUTxO @Tx u
      }

  u = generateWith (genUTxOSized n) 42

mkGreetings :: Greetings Tx
mkGreetings =
  (generateWith (arbitrary @(Greetings Tx)) 42)
    { snapshotUtxo = Just $ generateWith (genUTxOSized 100) 42
    }

sizeRow :: String -> String -> String -> String
sizeRow name jsonSize cborSize =
  "  " <> pad 44 name <> pad 10 jsonSize <> cborSize
 where
  pad n s = s <> replicate (max 1 (n - length s)) ' '

benchApplyTxs :: (UTxO, Tx) -> Either (Tx, ValidationError) UTxO
benchApplyTxs (utxo, tx) = applyTransactions defaultLedger (ChainSlot 1) utxo [tx]

defaultLedger :: Ledger Tx
defaultLedger = cardanoLedger defaultGlobals defaultLedgerEnv
