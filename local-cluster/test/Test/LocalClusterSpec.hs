module Test.LocalClusterSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Api (
  Address,
  Lovelace,
  MultiAssetSupportedInEra (MultiAssetInAlonzoEra),
  ShelleyAddr,
  SlotNo (SlotNo),
  TxIn (TxIn),
  TxIx (TxIx),
  TxOut (TxOut),
  TxOutDatumHash (TxOutDatumHashNone),
  TxOutValue (TxOutAdaOnly, TxOutValue),
  UTxO (..),
  lovelaceToValue,
  selectLovelace,
  serialiseToBech32,
  serialiseToRawBytesHexText,
  shelleyAddressInEra,
  writeFileTextEnvelope,
 )
import Cardano.Api.Shelley (Lovelace (Lovelace))
import CardanoClient (Sizes (..), buildAddress, buildRaw, calculateMinFee, defaultSizes, queryProtocolParameters, queryTipSlotNo, queryUtxo)
import CardanoCluster (ClusterConfig (..), ClusterLog (..), RunningCluster (..), keysFor, testClusterConfig, withCluster)
import CardanoNode (ChainTip (..), RunningNode (..), cliQueryTip)
import qualified Data.Map as Map
import Data.Text (unpack)
import Hydra.Logging (Tracer, showLogsOnFailure)
import qualified Paths_local_cluster as Pkg
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process

spec :: Spec
spec =
  it "should produce blocks and provide funds" $ do
    showLogsOnFailure $ \tr ->
      withTempDir "hydra-local-cluster" $ \tmp -> do
        let config = testClusterConfig tmp
        withCluster tr config $ \cluster -> do
          failAfter 30 $ assertNetworkIsProducingBlock tr cluster
          assertCanSpendInitialFunds cluster

assertNetworkIsProducingBlock :: Tracer IO ClusterLog -> RunningCluster -> IO ()
assertNetworkIsProducingBlock tracer = go (-1)
 where
  go blk cluster = case cluster of
    RunningCluster _ (RunningNode nodeId socket : _) -> do
      waitForNewBlock
      tip <- cliQueryTip (contramap (MsgFromNode nodeId) tracer) socket
      if block tip > blk
        then pure ()
        else go (block tip) cluster
    _ ->
      error "empty cluster?"

assertCanSpendInitialFunds :: HasCallStack => RunningCluster -> IO ()
assertCanSpendInitialFunds = \case
  cluster@(RunningCluster ClusterConfig{parentStateDirectory, networkId} (RunningNode nodeId socket : _)) -> do
    (vk, _) <- keysFor "alice" cluster
    addr <- buildAddress vk networkId
    UTxO utxo <- queryUtxo networkId socket [addr]
    let (txIn, TxOut _ val _) = case Map.toList utxo of
          [] -> error "No Utxo found"
          (tx : _) -> tx
        -- NOTE(AB): this is txOutValueToLovelace in more recent cardano-api versions
        amount = case val of
          TxOutAdaOnly _ l -> l
          TxOutValue _ v -> selectLovelace v

        nodeDirectory = parentStateDirectory </> "node-" <> show nodeId
        paymentOutput = TxOut (shelleyAddressInEra addr) (TxOutValue MultiAssetInAlonzoEra (lovelaceToValue 100_000_000)) TxOutDatumHashNone
    rawTx <- buildRaw [txIn] [] 0 0
    pparams <- queryProtocolParameters networkId socket
    let fee = calculateMinFee networkId rawTx defaultSizes{inputs = 1, outputs = 2, witnesses = 1} pparams
    slotNo <- queryTipSlotNo networkId socket
    let changeOutput = TxOut (shelleyAddressInEra addr) (TxOutValue MultiAssetInAlonzoEra (lovelaceToValue $ amount - 100_000_000 - fee)) TxOutDatumHashNone
        draftTxPath = nodeDirectory </> "tx.draft"
    draftTx <- buildRaw [txIn] [paymentOutput, changeOutput] (slotNo + 100) fee
    writeFileTextEnvelope draftTxPath Nothing draftTx >>= either (error . show) pure
    runTestScript nodeDirectory addr txIn amount slotNo fee draftTxPath socket
  _ ->
    error "empty cluster?"

runTestScript :: FilePath -> Address ShelleyAddr -> TxIn -> Lovelace -> SlotNo -> Lovelace -> FilePath -> FilePath -> IO ()
runTestScript nodeDirectory addr (TxIn txId (TxIx txIx)) (Lovelace amount) (SlotNo slot) (Lovelace fee) draftTxPath socket = do
  inputScript <- Pkg.getDataFileName "test_submit.sh"
  currentEnv <- getEnvironment
  let scriptOutput = nodeDirectory </> "test_submit.out"
  withFile' scriptOutput $ \fileOut ->
    withCreateProcess (sh currentEnv inputScript fileOut) $ \_stdin _stdout _stderr hdl ->
      waitForProcess hdl >>= \case
        ExitFailure{} -> readFile scriptOutput >>= failure . ("Initial funds spending script failed, output: " <>)
        ExitSuccess -> pure ()
 where
  socketEnv = ("CARDANO_NODE_SOCKET_PATH", socket)
  sh baseEnv script out =
    ( proc
        "/bin/sh"
        [ script
        , unpack $ serialiseToBech32 addr
        , -- NOTE(AB): there is a renderTxIn function in the API which is not exposed (yet?)
          unpack $ serialiseToRawBytesHexText txId <> "#" <> show txIx
        , show amount
        , show fee
        , show slot
        , draftTxPath
        ]
    )
      { env = Just (socketEnv : baseEnv)
      , cwd = Just nodeDirectory
      , std_out = UseHandle out
      , std_err = UseHandle out
      }

waitForNewBlock :: IO ()
waitForNewBlock = threadDelay (2 * slotLength)

slotLength :: DiffTime
slotLength = 1 -- FIXME this should be found in the genesis file

sshow :: (IsString s, Show a) => a -> s
sshow = fromString . show
