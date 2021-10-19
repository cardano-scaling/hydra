-- | A basic cardano-node client that can talk to a local cardano-node.
--
-- The idea of this module is to provide a Haskell interface on top of cardano-cli's API.
module CardanoClient where

import Hydra.Prelude

import Cardano.Api (
  Address,
  AddressAny (AddressShelley),
  AlonzoEra,
  CardanoEra (AlonzoEra),
  ConsensusModeParams (CardanoModeParams),
  EpochSlots (EpochSlots),
  EraInMode (AlonzoEraInCardanoMode),
  LocalNodeConnectInfo (LocalNodeConnectInfo),
  NetworkId,
  QueryInEra (QueryInShelleyBasedEra),
  QueryInMode (QueryInEra),
  QueryInShelleyBasedEra (QueryUTxO),
  QueryUTxOFilter (QueryUTxOByAddress),
  ShelleyAddr,
  UTxO,
 )
import Cardano.Api.Shelley (ShelleyBasedEra (ShelleyBasedEraAlonzo), VerificationKey (PaymentVerificationKey))
import Cardano.CLI.Shelley.Run.Address (buildShelleyAddress)
import Cardano.CLI.Shelley.Run.Query (executeQuery)
import qualified Cardano.Ledger.Keys as Keys
import qualified Data.Set as Set
import qualified Hydra.Chain.Direct.Wallet as Hydra

type NodeSocket = FilePath

-- | Build an address give a key.
-- From <runAddressBuild https://github.com/input-output-hk/cardano-node/blob/master/cardano-cli/src/Cardano/CLI/Shelley/Run/Address.hs#L106>
buildAddress :: Hydra.VerificationKey -> NetworkId -> IO (Address ShelleyAddr)
buildAddress vKey networkId = do
  let shelleyKey = PaymentVerificationKey $ Keys.VKey vKey
  runExceptT (buildShelleyAddress shelleyKey Nothing networkId) >>= \case
    Left err -> throwIO $ BuildAddressException (show err)
    Right addr -> pure addr

data BuildAddressException = BuildAddressException Text | QueryException Text
  deriving (Show)

instance Exception BuildAddressException

-- |Query UTxO for all given addresses.
--
-- This query is specialised for Shelley addresses in Alonzo era.
queryUtxo :: NetworkId -> FilePath -> [Address ShelleyAddr] -> IO (UTxO AlonzoEra)
queryUtxo networkId socket addresses =
  let query =
        QueryInEra
          AlonzoEraInCardanoMode
          ( QueryInShelleyBasedEra
              ShelleyBasedEraAlonzo
              ( QueryUTxO
                  (QueryUTxOByAddress (Set.fromList $ map AddressShelley addresses))
              )
          )
      defaultByronEpochSlots = 21600 :: Word64
      cardanoModeParams = CardanoModeParams $ EpochSlots defaultByronEpochSlots
      localNodeConnectInfo = LocalNodeConnectInfo cardanoModeParams networkId socket
   in runExceptT (executeQuery AlonzoEra cardanoModeParams localNodeConnectInfo query) >>= \case
        Left err -> throwIO $ QueryException (show err)
        Right utxo -> pure utxo
