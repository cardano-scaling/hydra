module Test.CardanoClientSpec where

import Cardano.Api (NetworkId (Testnet), NetworkMagic (NetworkMagic))
import qualified Cardano.Ledger.Keys as Ledger
import CardanoClient (buildAddress)
import Hydra.Ledger.Cardano (genKeyPair)
import Hydra.Prelude
import Test.Hydra.Prelude
import Test.QuickCheck.Monadic (PropertyM, forAllM, monadicIO, run)

spec :: Spec
spec = do
  prop "can generate address from verification key" $ monadicIO generateAddressFromKey

generateAddressFromKey :: PropertyM IO ()
generateAddressFromKey =
  forAllM genKeyPair $ \(Ledger.KeyPair (Ledger.VKey key) _) -> do
    void $ run $ buildAddress key (Testnet $ NetworkMagic 42)
