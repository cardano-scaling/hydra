module CardanoCluster where

import Hydra.Prelude

import Cardano.Api (
  AsType (..),
  HasTextEnvelope,
  PaymentKey,
  SigningKey (PaymentSigningKey),
  VerificationKey,
  readFileTextEnvelope,
 )
import Cardano.Crypto.DSIGN (deriveVerKeyDSIGN)
import CardanoNode (
  CardanoNodeArgs (..),
  CardanoNodeConfig (..),
  NodeId,
  NodeLog,
  Port,
  PortsConfig (..),
  RunningNode,
  defaultCardanoNodeArgs,
  withCardanoNode,
 )
import Control.Tracer (Tracer, traceWith)
import qualified Hydra.Chain.Direct.Wallet as Wallet
import System.Directory (copyFile, createDirectoryIfMissing)
import System.FilePath ((</>), (<.>))
import System.Posix.Files (
  ownerReadMode,
  setFileMode,
 )
import Test.Network.Ports (randomUnusedTCPPorts)

data RunningCluster = RunningCluster ClusterConfig [RunningNode]

-- | Configuration parameters for the cluster.
newtype ClusterConfig = ClusterConfig {parentStateDirectory :: FilePath}

keysFor :: String -> RunningCluster -> IO (Wallet.VerificationKey, Wallet.SigningKey)
keysFor actor (RunningCluster (ClusterConfig dir) _) = do
  PaymentSigningKey sk <- readFileTextEnvelopeThrow asSigningKey (dir </> actor <.> "sk")
  let vk = deriveVerKeyDSIGN sk
  pure (vk, sk)

readFileTextEnvelopeThrow ::
  HasTextEnvelope a =>
  AsType a ->
  FilePath ->
  IO a
readFileTextEnvelopeThrow asType =
  either (fail . show) pure <=< readFileTextEnvelope asType

asSigningKey :: AsType (SigningKey PaymentKey)
asSigningKey = AsSigningKey AsPaymentKey

withCluster ::
  Tracer IO ClusterLog -> ClusterConfig -> (RunningCluster -> IO ()) -> IO ()
withCluster tr cfg@ClusterConfig{parentStateDirectory} action = do
  systemStart <- initSystemStart
  (cfgA, cfgB, cfgC) <-
    makeNodesConfig parentStateDirectory systemStart
      <$> randomUnusedTCPPorts 3
  withBFTNode tr cfgA $ \nodeA -> do
    withBFTNode tr cfgB $ \nodeB -> do
      withBFTNode tr cfgC $ \nodeC -> do
        action (RunningCluster cfg [nodeA, nodeB, nodeC])

withBFTNode :: Tracer IO ClusterLog -> CardanoNodeConfig -> (RunningNode -> IO ()) -> IO ()
withBFTNode clusterTracer cfg action = do
  createDirectoryIfMissing False (stateDirectory cfg)

  [dlgCert, signKey, vrfKey, kesKey, opCert, _utxoSkey, _utxoVkey] <-
    forM
      [ dlgCertFilename nid
      , signKeyFilename nid
      , vrfKeyFilename nid
      , kesKeyFilename nid
      , opCertFilename nid
      , -- Keys for the initial funds
        utxoSigningKey
      , utxoVerificationKey
      ]
      (copyCredential (stateDirectory cfg))

  let args =
        defaultCardanoNodeArgs
          { nodeDlgCertFile = Just dlgCert
          , nodeSignKeyFile = Just signKey
          , nodeVrfKeyFile = Just vrfKey
          , nodeKesKeyFile = Just kesKey
          , nodeOpCertFile = Just opCert
          , nodePort = Just (ours (ports cfg))
          }

  copyFile
    ("config" </> "cardano-node.json")
    (stateDirectory cfg </> nodeConfigFile args)

  copyFile
    ("config" </> "genesis-byron.json")
    (stateDirectory cfg </> nodeByronGenesisFile args)

  copyFile
    ("config" </> "genesis-shelley.json")
    (stateDirectory cfg </> nodeShelleyGenesisFile args)

  copyFile
    ("config" </> "genesis-alonzo.json")
    (stateDirectory cfg </> nodeAlonzoGenesisFile args)

  withCardanoNode nodeTracer cfg args $ \rn -> do
    traceWith clusterTracer $ MsgNodeStarting cfg
    action rn
 where
  dlgCertFilename i = "delegation-cert.00" <> show (i - 1) <> ".json"
  signKeyFilename i = "delegate-keys.00" <> show (i - 1) <> ".key"
  vrfKeyFilename i = "delegate" <> show i <> ".vrf.skey"
  kesKeyFilename i = "delegate" <> show i <> ".kes.skey"
  opCertFilename i = "opcert" <> show i <> ".cert"
  utxoVerificationKey = "utxo1.vkey"
  utxoSigningKey = "utxo1.skey"

  copyCredential parentDir file = do
    let source = "config" </> "credentials" </> file
    let destination = parentDir </> file
    copyFile source destination
    setFileMode destination ownerReadMode
    pure destination

  nid = nodeId cfg

  nodeTracer = contramap (MsgFromNode nid) clusterTracer

-- | Initialize the system start time to now (modulo a small offset needed to
-- give time to the system to bootstrap correctly).
initSystemStart :: IO UTCTime
initSystemStart = do
  addUTCTime 1 <$> getCurrentTime

makeNodesConfig ::
  FilePath ->
  UTCTime ->
  [Port] ->
  (CardanoNodeConfig, CardanoNodeConfig, CardanoNodeConfig)
makeNodesConfig stateDirectory systemStart [a, b, c] =
  ( CardanoNodeConfig 1 (stateDirectory </> "node-1") systemStart $
      PortsConfig a [b, c]
  , CardanoNodeConfig 2 (stateDirectory </> "node-2") systemStart $
      PortsConfig b [a, c]
  , CardanoNodeConfig 3 (stateDirectory </> "node-3") systemStart $
      PortsConfig c [a, b]
  )
makeNodesConfig _ _ _ = error "we only support topology for 3 nodes"

--
-- Logging
--

data ClusterLog
  = MsgFromNode NodeId NodeLog
  | MsgNodeStarting CardanoNodeConfig
  deriving (Show)
