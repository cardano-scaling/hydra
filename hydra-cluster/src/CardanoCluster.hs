{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module CardanoCluster where

import Hydra.Cardano.Api
import Hydra.Prelude

import qualified Cardano.Api.UTxO as UTxO
import Cardano.Ledger.Keys (VKey (VKey))
import CardanoClient (
  CardanoClientException,
  QueryPoint (QueryTip),
  build,
  buildAddress,
  queryUTxO,
  sign,
  submit,
  waitForPayment,
 )
import CardanoNode (
  CardanoNodeConfig (..),
  NodeId,
  NodeLog,
  Port,
  PortsConfig (..),
  RunningNode (..),
  initSystemStart,
  withBFTNode,
 )
import Control.Tracer (Tracer)
import qualified Data.Aeson as Aeson
import Hydra.Chain.Direct.Util (markerDatumHash, retry)
import qualified Hydra.Chain.Direct.Util as Cardano
import Hydra.Cluster.Util (readConfigFile)
import Hydra.Options (
  ChainConfig (..),
  defaultChainConfig,
 )
import System.FilePath ((<.>), (</>))
import Test.Network.Ports (randomUnusedTCPPorts)

-- | TODO: This is hard-coded and must match what's in the genesis file, so
-- ideally, we want to either:
--
-- - overwrite the genesis configuration with the `ClusterConfig`
-- - pull the network id from the genesis configuration
defaultNetworkId :: NetworkId
defaultNetworkId = Testnet (NetworkMagic 42)

-- NOTE: This is hard-coded and needs to correspond to the initial funds set in
-- the genesis-shelley.json file.
availableInitialFunds :: Num a => a
availableInitialFunds = 900_000_000_000

-- | Enumeration of known actors for which we can get the 'keysFor' and 'writeKeysFor'.
data Actor
  = Alice
  | Bob
  | Carol
  | Faucet

actorName :: Actor -> String
actorName = \case
  Alice -> "alice"
  Bob -> "bob"
  Carol -> "carol"
  Faucet -> "faucet"

-- | Get the "well-known" keys for given actor.
keysFor :: Actor -> IO (VerificationKey PaymentKey, SigningKey PaymentKey)
keysFor actor = do
  bs <- readConfigFile ("credentials" </> actorName actor <.> "sk")
  let res =
        first TextEnvelopeAesonDecodeError (Aeson.eitherDecodeStrict bs)
          >>= deserialiseFromTextEnvelope asSigningKey
  case res of
    Left err ->
      fail $ "cannot decode text envelope from '" <> show bs <> "', error: " <> show err
    Right sk -> pure (getVerificationKey sk, sk)

fromRawVKey :: Cardano.VerificationKey -> VerificationKey PaymentKey
fromRawVKey = PaymentVerificationKey . VKey

chainConfigFor :: Actor -> FilePath -> FilePath -> [Actor] -> IO ChainConfig
chainConfigFor me targetDir nodeSocket them = do
  readConfigFile ("credentials" </> skName me) >>= writeFileBS (skTarget me)
  readConfigFile ("credentials" </> vkName me) >>= writeFileBS (vkTarget me)
  pure $
    defaultChainConfig
      { nodeSocket
      , cardanoSigningKey = skTarget me
      , cardanoVerificationKeys = [vkTarget himOrHer | himOrHer <- them]
      }
 where
  skTarget x = targetDir </> skName x
  vkTarget x = targetDir </> vkName x
  skName x = actorName x <.> ".sk"
  vkName x = actorName x <.> ".vk"
-- * Starting a cluster or single nodes

data RunningCluster = RunningCluster ClusterConfig [RunningNode]

-- | Configuration parameters for the cluster.
data ClusterConfig = ClusterConfig
  { parentStateDirectory :: FilePath
  , networkId :: NetworkId
  }

asSigningKey :: AsType (SigningKey PaymentKey)
asSigningKey = AsSigningKey AsPaymentKey

withCluster ::
  Tracer IO ClusterLog -> ClusterConfig -> (RunningCluster -> IO ()) -> IO ()
withCluster tr cfg@ClusterConfig{parentStateDirectory} action = do
  systemStart <- initSystemStart
  (cfgA, cfgB, cfgC) <-
    makeNodesConfig parentStateDirectory systemStart
      <$> randomUnusedTCPPorts 3

  withBFTNode (nodeTracer cfgA) cfgA $ \nodeA -> do
    withBFTNode (nodeTracer cfgB) cfgB $ \nodeB -> do
      withBFTNode (nodeTracer cfgC) cfgC $ \nodeC -> do
        let nodes = [nodeA, nodeB, nodeC]
        action (RunningCluster cfg nodes)
 where
  nodeTracer CardanoNodeConfig{nodeId} = contramap (MsgFromNode nodeId) tr

  makeNodesConfig ::
    FilePath ->
    UTCTime ->
    [Port] ->
    (CardanoNodeConfig, CardanoNodeConfig, CardanoNodeConfig)
  makeNodesConfig stateDirectory systemStart [a, b, c] =
    ( CardanoNodeConfig 1 (stateDirectory </> "node-1") systemStart (PortsConfig a [b, c])
    , CardanoNodeConfig 2 (stateDirectory </> "node-2") systemStart (PortsConfig b [a, c])
    , CardanoNodeConfig 3 (stateDirectory </> "node-3") systemStart (PortsConfig c [a, b])
    )
  makeNodesConfig _ _ _ = error "we only support topology for 3 nodes"

data Marked = Fuel | Normal

-- | Create a specially marked "seed" UTXO containing requested 'Lovelace' by
-- redeeming funds available to the well-known faucet.
--
-- NOTE: This function is querying and looping forever until it finds a suitable
-- output!
seedFromFaucet ::
  NetworkId ->
  RunningNode ->
  -- | Recipient of the funds
  VerificationKey PaymentKey ->
  -- | Amount to get from faucet
  Lovelace ->
  -- | Marked as fuel or normal output?
  Marked ->
  IO UTxO
seedFromFaucet networkId (RunningNode _ nodeSocket) receivingVerificationKey lovelace marked = do
  (faucetVk, faucetSk) <- keysFor Faucet
  retry isCardanoClientException $ submitFuelingTx faucetVk faucetSk
  waitForPayment networkId nodeSocket lovelace receivingAddress
 where
  submitFuelingTx faucetVk faucetSk = do
    (i, _o) <- findUTxO faucetVk
    let changeAddress = buildAddress faucetVk networkId
    build networkId nodeSocket changeAddress [(i, Nothing)] [] [theOutput] >>= \case
      Left e -> error (show e)
      Right body -> do
        submit networkId nodeSocket (sign faucetSk body)

  findUTxO faucetVk = do
    faucetUTxO <- queryUTxO networkId nodeSocket QueryTip [buildAddress faucetVk networkId]
    let foundUTxO = find (\(_i, o) -> txOutLovelace o >= lovelace) $ UTxO.pairs faucetUTxO
    case foundUTxO of
      Just o -> pure o
      Nothing ->
        findUTxO faucetVk

  receivingAddress = buildAddress receivingVerificationKey networkId

  theOutput =
    TxOut
      (shelleyAddressInEra receivingAddress)
      (lovelaceToValue lovelace)
      theOutputDatum

  theOutputDatum = case marked of
    Fuel -> TxOutDatumHash markerDatumHash
    Normal -> TxOutDatumNone

  isCardanoClientException :: CardanoClientException -> Bool
  isCardanoClientException = const True

-- | Like 'seedFromFaucet', but without returning the seeded 'UTxO'.
seedFromFaucet_ ::
  NetworkId ->
  RunningNode ->
  -- | Recipient of the funds
  VerificationKey PaymentKey ->
  -- | Amount to get from faucet
  Lovelace ->
  -- | Marked as fuel or normal output?
  Marked ->
  IO ()
seedFromFaucet_ nid node vk ll marked =
  void $ seedFromFaucet nid node vk ll marked

--
-- Logging
--

data ClusterLog
  = MsgFromNode NodeId NodeLog
  | MsgNodeStarting CardanoNodeConfig
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
