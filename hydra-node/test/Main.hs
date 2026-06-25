module Main where

import Hydra.Prelude

import Hydra.API.ClientInputSpec qualified
import Hydra.API.HTTPServerSpec qualified
import Hydra.API.ServerOutputSpec qualified
import Hydra.API.ServerSpec qualified
import Hydra.BehaviorSpec qualified
import Hydra.Chain.BlockfrostSpec qualified
import Hydra.Chain.Direct.HandlersSpec qualified
import Hydra.Chain.Direct.ScriptRegistrySpec qualified
import Hydra.Chain.Direct.StateSpec qualified
import Hydra.Chain.Direct.TimeHandleSpec qualified
import Hydra.Chain.Direct.TxSpec qualified
import Hydra.Chain.Direct.TxTraceSpec qualified
import Hydra.Chain.Direct.WalletSpec qualified
import Hydra.Chain.ScriptRegistrySpec qualified
import Hydra.ConfigSpec qualified
import Hydra.CryptoSpec qualified
import Hydra.Events.RotationSpec qualified
import Hydra.Events.S3Spec qualified
import Hydra.Events.SQLiteBasedSpec qualified
import Hydra.Events.UDPSpec qualified
import Hydra.HeadLogicSnapshotSpec qualified
import Hydra.HeadLogicSpec qualified
import Hydra.JSONSchemaSpec qualified
import Hydra.Ledger.Cardano.TimeSpec qualified
import Hydra.Ledger.CardanoSpec qualified
import Hydra.Ledger.SimpleSpec qualified
import Hydra.Logging.MonitoringSpec qualified
import Hydra.LoggingSpec qualified
import Hydra.Model.MockChainSpec qualified
import Hydra.ModelSpec qualified
import Hydra.Network.AuthenticateSpec qualified
import Hydra.NetworkSpec qualified
import Hydra.NetworkVersionsSpec qualified
import Hydra.Node.InputQueueSpec qualified
import Hydra.Node.RunSpec qualified
import Hydra.NodeSpec qualified
import Hydra.OffChainLeaderSpec qualified
import Hydra.OptionsSpec qualified
import Hydra.PartySpec qualified
import Hydra.PersistentQueueSpec qualified
import Hydra.UtilsSpec qualified
import Test.Hydra.TastyMain (defaultMainHydra, testSpec)

main :: IO ()
main =
  defaultMainHydra
    "hydra-node"
    [ testSpec "API.ClientInput" Hydra.API.ClientInputSpec.spec
    , testSpec "API.HTTPServer" Hydra.API.HTTPServerSpec.spec
    , testSpec "API.ServerOutput" Hydra.API.ServerOutputSpec.spec
    , testSpec "API.Server" Hydra.API.ServerSpec.spec
    , testSpec "Behavior" Hydra.BehaviorSpec.spec
    , testSpec "Chain.Blockfrost" Hydra.Chain.BlockfrostSpec.spec
    , testSpec "Chain.Direct.Handlers" Hydra.Chain.Direct.HandlersSpec.spec
    , testSpec "Chain.Direct.ScriptRegistry" Hydra.Chain.Direct.ScriptRegistrySpec.spec
    , testSpec "Chain.Direct.State" Hydra.Chain.Direct.StateSpec.spec
    , testSpec "Chain.Direct.TimeHandle" Hydra.Chain.Direct.TimeHandleSpec.spec
    , testSpec "Chain.Direct.Tx" Hydra.Chain.Direct.TxSpec.spec
    , testSpec "Chain.Direct.TxTrace" Hydra.Chain.Direct.TxTraceSpec.spec
    , testSpec "Chain.Direct.Wallet" Hydra.Chain.Direct.WalletSpec.spec
    , testSpec "Chain.ScriptRegistry" Hydra.Chain.ScriptRegistrySpec.spec
    , testSpec "Config" Hydra.ConfigSpec.spec
    , testSpec "Crypto" Hydra.CryptoSpec.spec
    , testSpec "Events.Rotation" Hydra.Events.RotationSpec.spec
    , testSpec "Events.S3" Hydra.Events.S3Spec.spec
    , testSpec "Events.SQLiteBased" Hydra.Events.SQLiteBasedSpec.spec
    , testSpec "Events.UDP" Hydra.Events.UDPSpec.spec
    , testSpec "HeadLogicSnapshot" Hydra.HeadLogicSnapshotSpec.spec
    , testSpec "HeadLogic" Hydra.HeadLogicSpec.spec
    , testSpec "JSONSchema" Hydra.JSONSchemaSpec.spec
    , testSpec "Ledger.Cardano" Hydra.Ledger.CardanoSpec.spec
    , testSpec "Ledger.Cardano.Time" Hydra.Ledger.Cardano.TimeSpec.spec
    , testSpec "Ledger.Simple" Hydra.Ledger.SimpleSpec.spec
    , testSpec "Logging.Monitoring" Hydra.Logging.MonitoringSpec.spec
    , testSpec "Logging" Hydra.LoggingSpec.spec
    , testSpec "Model.MockChain" Hydra.Model.MockChainSpec.spec
    , testSpec "Model" Hydra.ModelSpec.spec
    , testSpec "Network.Authenticate" Hydra.Network.AuthenticateSpec.spec
    , testSpec "Network" Hydra.NetworkSpec.spec
    , testSpec "NetworkVersions" Hydra.NetworkVersionsSpec.spec
    , testSpec "Node.InputQueue" Hydra.Node.InputQueueSpec.spec
    , testSpec "Node.Run" Hydra.Node.RunSpec.spec
    , testSpec "Node" Hydra.NodeSpec.spec
    , testSpec "OffChainLeader" Hydra.OffChainLeaderSpec.spec
    , testSpec "Options" Hydra.OptionsSpec.spec
    , testSpec "Party" Hydra.PartySpec.spec
    , testSpec "PersistentQueue" Hydra.PersistentQueueSpec.spec
    , testSpec "Utils" Hydra.UtilsSpec.spec
    ]
