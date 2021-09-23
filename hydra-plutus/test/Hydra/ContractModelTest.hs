{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Hydra.ContractModelTest where

import Hydra.Prelude as Prelude

import GHC.Show (Show (..))
import qualified Hydra.Depreciated.OffChain as OffChain
import qualified Hydra.Depreciated.OnChain as OnChain
import Ledger (pubKeyHash)
import Ledger.Ada as Ada
import Ledger.Typed.Scripts (MintingPolicy)
import Plutus.Contract (Contract)
import Plutus.Contract.Test (Wallet, walletPubKey)
import Plutus.Contract.Test.ContractModel
import Plutus.Contract.Trace (Wallet (Wallet))
import Plutus.Contract.Types (ContractError)
import Test.QuickCheck (Property, Testable (property))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

w1, w2, w3 :: Wallet
[w1, w2, w3] = map Wallet [1, 2, 3]

wallets :: [Wallet]
wallets = [w1, w2, w3]

data HeadState = Initialised | Committing | Opened | Closed
  deriving (Eq, Show)

data HydraModel = HydraModel {headState :: Maybe HeadState}
  deriving (Eq, Show)

instance ContractModel HydraModel where
  data Action HydraModel
    = Init Wallet
    | Commit Wallet Ada.Ada
    | CollectCom Wallet
    | Close Wallet
    deriving (Eq, Show)

  data ContractInstanceKey HydraModel w schema err where
    -- | No party in a Hydra Head is privileged
    HeadParty :: Wallet -> ContractInstanceKey HydraModel [OnChain.State] OffChain.Schema ContractError

  arbitraryAction _ = pure (Init w1)
  initialState = HydraModel Nothing
  nextState _ = pure ()

instance Eq (ContractInstanceKey HydraModel w schema err) where
  HeadParty w == HeadParty w' = w == w'

instance Show (ContractInstanceKey HydraModel w schema err) where
  show (HeadParty w) = "HeadParty " <> Prelude.show w

-- Defines all the contract instances that will run as part of a test
instanceSpec :: [ContractInstanceSpec HydraModel]
instanceSpec = [ContractInstanceSpec (HeadParty w) w hydraContract | w <- wallets]
 where
  testPolicy :: MintingPolicy
  testPolicy = OnChain.hydraMintingPolicy 42

  hydraContract :: Contract [OnChain.State] OffChain.Schema ContractError ()
  hydraContract = OffChain.contract headParameters
   where
    headParameters =
      OffChain.mkHeadParameters
        (map (pubKeyHash . walletPubKey) wallets)
        testPolicy

prop_HydraOCV :: Actions HydraModel -> Property
prop_HydraOCV = propRunActions_ instanceSpec

tests :: TestTree
tests =
  testGroup
    "Model Based Testing of Hydra Head"
    [testProperty "Hydra On-Chain Validation Protocol" $ property prop_HydraOCV]
