{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}

module CardanoCluster where

import Hydra.Prelude

import Cardano.Ledger.Keys (VKey (VKey))
import CardanoClient (buildAddress)
import CardanoNode (
  CardanoNodeArgs (..),
  CardanoNodeConfig (..),
  NodeId,
  NodeLog,
  Port,
  PortsConfig (..),
  RunningNode (..),
  addField,
  defaultCardanoNodeArgs,
  withCardanoNode,
 )
import Control.Lens ((.~))
import Control.Tracer (Tracer, traceWith)
import Data.Aeson (object)
import qualified Data.Aeson as Aeson
import Data.Aeson.Lens (key)
import qualified Data.ByteString as BS
import Data.ByteString.Base16 (encodeBase16)
import Hydra.Cardano.Api (
  AsType (..),
  NetworkId (Testnet),
  NetworkMagic (NetworkMagic),
  PaymentKey,
  SigningKey,
  TextEnvelopeError (TextEnvelopeAesonDecodeError),
  VerificationKey (PaymentVerificationKey),
  deserialiseFromTextEnvelope,
  getVerificationKey,
  serialiseToRawBytes,
 )
import qualified Hydra.Chain.Direct.Util as Cardano
import qualified Paths_hydra_cluster as Pkg
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((<.>), (</>))
import System.Posix.Files (
  ownerReadMode,
  setFileMode,
 )
import Test.Network.Ports (randomUnusedTCPPort, randomUnusedTCPPorts)

data RunningCluster = RunningCluster ClusterConfig [RunningNode]

-- | TODO: This is hard-coded and must match what's in the genesis file, so
-- ideally, we want to either:
--
-- - overwrite the genesis configuration with the `ClusterConfig`
-- - pull the network id from the genesis configuration
defaultNetworkId :: NetworkId
defaultNetworkId = Testnet (NetworkMagic 42)

-- FIXME: This is hard-coded and should correspond to the initial funds set in
-- the genesis file.
availableInitialFunds :: Num a => a
availableInitialFunds = 900_000_000_000

-- | Configuration parameters for the cluster.
data ClusterConfig = ClusterConfig
  { parentStateDirectory :: FilePath
  , networkId :: NetworkId
  , initialFunds :: [VerificationKey PaymentKey]
  }

asSigningKey :: AsType (SigningKey PaymentKey)
asSigningKey = AsSigningKey AsPaymentKey

withCluster ::
  Tracer IO ClusterLog -> ClusterConfig -> (RunningCluster -> IO ()) -> IO ()
withCluster tr cfg@ClusterConfig{parentStateDirectory, initialFunds} action = do
  systemStart <- initSystemStart
  (cfgA, cfgB, cfgC) <-
    makeNodesConfig parentStateDirectory systemStart
      <$> randomUnusedTCPPorts 3

  withBFTNode tr cfgA initialFunds $ \nodeA -> do
    withBFTNode tr cfgB initialFunds $ \nodeB -> do
      withBFTNode tr cfgC initialFunds $ \nodeC -> do
        let nodes = [nodeA, nodeB, nodeC]
        action (RunningCluster cfg nodes)

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

-- | Write the "well-known" keys for given actor into a target directory.
writeKeysFor ::
  -- | Target directory
  FilePath ->
  Actor ->
  -- | Paths of written keys in the form of (verification key, signing key)
  IO (FilePath, FilePath)
writeKeysFor targetDir actor = do
  readConfigFile ("credentials" </> skName) >>= writeFileBS skTarget
  readConfigFile ("credentials" </> vkName) >>= writeFileBS vkTarget
  pure (vkTarget, skTarget)
 where
  skTarget = targetDir </> skName

  vkTarget = targetDir </> vkName

  skName = actorName actor <.> ".sk"

  vkName = actorName actor <.> ".vk"

withBFTNode ::
  Tracer IO ClusterLog ->
  CardanoNodeConfig ->
  [VerificationKey PaymentKey] ->
  (RunningNode -> IO ()) ->
  IO ()
withBFTNode clusterTracer cfg initialFunds action = do
  createDirectoryIfMissing False (stateDirectory cfg)

  [dlgCert, signKey, vrfKey, kesKey, opCert] <-
    forM
      [ dlgCertFilename nid
      , signKeyFilename nid
      , vrfKeyFilename nid
      , kesKeyFilename nid
      , opCertFilename nid
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

  readConfigFile "cardano-node.json"
    >>= writeFileBS
      (stateDirectory cfg </> nodeConfigFile args)

  readConfigFile "genesis-byron.json"
    >>= writeFileBS
      (stateDirectory cfg </> nodeByronGenesisFile args)

  setInitialFundsInGenesisShelley (stateDirectory cfg </> nodeShelleyGenesisFile args)

  readConfigFile "genesis-alonzo.json"
    >>= writeFileBS
      (stateDirectory cfg </> nodeAlonzoGenesisFile args)

  withCardanoNode nodeTracer cfg args $ \rn -> do
    traceWith clusterTracer $ MsgNodeStarting cfg
    waitForSocket rn
    action rn
 where
  dlgCertFilename i = "delegation-cert.00" <> show (i - 1) <> ".json"
  signKeyFilename i = "delegate-keys.00" <> show (i - 1) <> ".key"
  vrfKeyFilename i = "delegate" <> show i <> ".vrf.skey"
  kesKeyFilename i = "delegate" <> show i <> ".kes.skey"
  opCertFilename i = "opcert" <> show i <> ".cert"

  setInitialFundsInGenesisShelley file = do
    bs <- readConfigFile "genesis-shelley.json"
    genesisJson <- either fail pure $ Aeson.eitherDecodeStrict @Aeson.Value bs
    let updatedJson = genesisJson & key "initialFunds" .~ initialFundsValue
    Aeson.encodeFile file updatedJson

  initialFundsValue =
    foldr
      (uncurry addField)
      (object [])
      (mkInitialFundsEntry <$> initialFunds)

  mkInitialFundsEntry :: VerificationKey PaymentKey -> (Text, Word)
  mkInitialFundsEntry vk =
    let addr = buildAddress vk defaultNetworkId
        bytes = serialiseToRawBytes addr
     in (encodeBase16 bytes, availableInitialFunds)

  copyCredential parentDir file = do
    bs <- readConfigFile ("credentials" </> file)
    let destination = parentDir </> file
    writeFileBS destination bs
    setFileMode destination ownerReadMode
    pure destination

  nid = nodeId cfg

  nodeTracer = contramap (MsgFromNode nid) clusterTracer

  waitForSocket :: RunningNode -> IO ()
  waitForSocket node@(RunningNode _ socket) = do
    unlessM (doesFileExist socket) $ do
      threadDelay 0.1
      waitForSocket node

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
  ( CardanoNodeConfig 1 (stateDirectory </> "node-1") systemStart (PortsConfig a [b, c])
  , CardanoNodeConfig 2 (stateDirectory </> "node-2") systemStart (PortsConfig b [a, c])
  , CardanoNodeConfig 3 (stateDirectory </> "node-3") systemStart (PortsConfig c [a, b])
  )
makeNodesConfig _ _ _ = error "we only support topology for 3 nodes"

newNodeConfig ::
  FilePath ->
  IO CardanoNodeConfig
newNodeConfig stateDirectory = do
  nodePort <- randomUnusedTCPPort
  systemStart <- initSystemStart
  pure $
    CardanoNodeConfig
      { nodeId = 1
      , stateDirectory
      , systemStart
      , ports = PortsConfig nodePort []
      }

-- | Lookup a config file similar reading a file from disk.
readConfigFile :: FilePath -> IO ByteString
readConfigFile source = do
  filename <- Pkg.getDataFileName ("config" </> source)
  BS.readFile filename

--
-- Logging
--

data ClusterLog
  = MsgFromNode NodeId NodeLog
  | MsgNodeStarting CardanoNodeConfig
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
