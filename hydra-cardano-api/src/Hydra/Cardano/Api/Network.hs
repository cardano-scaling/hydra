module Hydra.Cardano.Api.Network (
  Network (..),
  networkIdToNetwork,
) where

import qualified Cardano.Api as Api
import Cardano.Ledger.BaseTypes (Network (..))

networkIdToNetwork :: Api.NetworkId -> Network
networkIdToNetwork Api.Mainnet = Mainnet
networkIdToNetwork (Api.Testnet _) = Testnet
