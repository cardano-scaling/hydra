-- | A basic cardano-node client that can talk to a local cardano-node.
--
-- The idea of this module is to provide a Haskell interface on top of cardano-cli's API.
module CardanoClient where

import Hydra.Prelude

import Cardano.Api (Address, NetworkId, ShelleyAddr)
import Cardano.Api.Shelley (VerificationKey (PaymentVerificationKey))
import Cardano.CLI.Shelley.Run.Address (buildShelleyAddress)
import qualified Cardano.Ledger.Keys as Keys
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

newtype BuildAddressException = BuildAddressException Text
  deriving (Show)

instance Exception BuildAddressException
