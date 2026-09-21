-- | Tests for the @?address=@ output filter applied by 'Hydra.API.WSServer'.
-- 'Hydra.API.ServerSpec' covers the server-side wiring that reaches it.
module Hydra.API.ServerOutputFilterSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Data.Text qualified as Text
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.API.ServerOutput (ServerOutput (..), TimedServerOutput (..))
import Hydra.API.ServerOutputFilter (ServerOutputFilter (..), serverOutputFilter)
import Hydra.Cardano.Api
import Hydra.Cardano.Api.Gen (genTxIn)
import Hydra.HeadLogicSpec (testSnapshot)
import Hydra.Ledger.Cardano.Builder (addTxInsSpending, unsafeBuildTransaction)
import Test.Hydra.API.ServerOutput ()
import Test.Hydra.Chain.Direct.State ()
import Test.Hydra.Tx.Fixture (testHeadId, testNetworkId)
import Test.Hydra.Tx.Gen (genKeyPair)
import Test.QuickCheck (generate)
import Test.QuickCheck.Arbitrary.ADT (ADTArbitrary (..), ConstructorArbitraryPair (..), toADTArbitrary)

spec :: Spec
spec = parallel $ do
  describe "SnapshotConfirmed" $ do
    it "passes a snapshot whose confirmed tx pays the address" $
      snapshotConfirmed [txPayingTo [aliceAddress]] `shouldPassFor` aliceAddress

    it "drops a snapshot when no confirmed tx involves the address" $
      snapshotConfirmed [txPayingTo [bobAddress]] `shouldDropFor` aliceAddress

    -- A snapshot confirming no transaction settles only a deposit or a
    -- decommit. It carries no address, so it is not withheld: the filter
    -- narrows what a client sees rather than hiding addressless events.
    it "passes a snapshot confirming no transactions" $
      snapshotConfirmed [] `shouldPassFor` aliceAddress

    it "passes when any one of several confirmed txs involves the address" $
      snapshotConfirmed [txPayingTo [bobAddress], txPayingTo [carolAddress, aliceAddress]]
        `shouldPassFor` aliceAddress

    it "passes when the address is any one of a tx's outputs" $
      snapshotConfirmed [txPayingTo [bobAddress, carolAddress, aliceAddress]]
        `shouldPassFor` aliceAddress

  -- The snapshot in which a client's funds leave the head is the one it most
  -- needs, and a transaction spending a UTxO in full leaves no output to match.
  describe "spending from the address" $ do
    it "passes a snapshot whose confirmed tx spends from the address" $
      snapshotConfirmed [aliceSpendsTo bobAddress] `shouldPassFor` aliceAddress

    it "still drops that snapshot for an uninvolved address" $
      snapshotConfirmed [aliceSpendsTo bobAddress] `shouldDropFor` carolAddress

    it "passes for both the sender and the recipient" $ do
      snapshotConfirmed [aliceSpendsTo bobAddress] `shouldPassFor` bobAddress
      snapshotConfirmed [aliceSpendsTo bobAddress] `shouldPassFor` aliceAddress

  describe "address comparison" $ do
    it "does not match a well-formed address that is not involved" $
      snapshotConfirmed [txPayingTo [aliceAddress, bobAddress]] `shouldDropFor` carolAddress

    it "does not match a string that is not an address" $ do
      snapshotConfirmed [txPayingTo [aliceAddress]] `shouldDrop` "invalid"
      snapshotConfirmed [txPayingTo [aliceAddress]] `shouldDrop` ""

    it "does not match an address truncated or extended by one character" $ do
      let addr = bech32Of aliceAddress
      snapshotConfirmed [txPayingTo [aliceAddress]] `shouldDrop` Text.init addr
      snapshotConfirmed [txPayingTo [aliceAddress]] `shouldDrop` (addr <> "x")

  -- Enumerated rather than sampled: adding a filtering arm for any other
  -- output would otherwise go unnoticed. Several of them do carry a
  -- transaction ('TxInvalid', 'DecommitRequested', 'DecommitInvalid'), so
  -- passing them is a decision, not a consequence of having nothing to match.
  it "passes every output other than SnapshotConfirmed" $ do
    others <- serverOutputsOtherThanSnapshotConfirmed
    length others `shouldSatisfy` (> 1)
    forM_ others $ \(name, out) ->
      unless (matches out (bech32Of carolAddress)) . failure $
        name <> " was filtered out"

-- * Fixtures

aliceKeys, bobKeys, carolKeys :: (VerificationKey PaymentKey, SigningKey PaymentKey)
aliceKeys = genKeyPair `generateWith` 1
bobKeys = genKeyPair `generateWith` 2
carolKeys = genKeyPair `generateWith` 3

aliceAddress, bobAddress, carolAddress :: AddressInEra
aliceAddress = mkVkAddress testNetworkId (fst aliceKeys)
bobAddress = mkVkAddress testNetworkId (fst bobKeys)
carolAddress = mkVkAddress testNetworkId (fst carolKeys)

-- | A transaction paying 2 Ada to each of the given addresses, unsigned.
txPayingTo :: [AddressInEra] -> Tx
txPayingTo addresses =
  unsafeBuildTransaction $
    defaultTxBodyContent
      & addTxInsSpending [genTxIn `generateWith` 42]
      & addTxOuts
        [ TxOut address (lovelaceToValue 2_000_000) TxOutDatumNone ReferenceScriptNone
        | address <- addresses
        ]

-- | Signed by Alice and paying only to the given address, so her witness is
-- all the filter has to go on.
aliceSpendsTo :: AddressInEra -> Tx
aliceSpendsTo recipient = signTx (snd aliceKeys) (txPayingTo [recipient])

snapshotConfirmed :: [Tx] -> ServerOutput Tx
snapshotConfirmed confirmed =
  SnapshotConfirmed
    { headId = testHeadId
    , snapshot = testSnapshot 1 0 confirmed mempty
    , signatures = mempty
    }

serverOutputsOtherThanSnapshotConfirmed :: IO [(String, ServerOutput Tx)]
serverOutputsOtherThanSnapshotConfirmed = do
  ADTArbitrary{adtCAPs} <- generate $ toADTArbitrary (Proxy @(ServerOutput Tx))
  pure
    [ (capConstructor, capArbitrary)
    | ConstructorArbitraryPair{capConstructor, capArbitrary} <- adtCAPs
    , capConstructor /= "SnapshotConfirmed"
    ]

-- * Helpers

shouldPassFor :: HasCallStack => ServerOutput Tx -> AddressInEra -> Expectation
shouldPassFor out = shouldPass out . bech32Of

shouldDropFor :: HasCallStack => ServerOutput Tx -> AddressInEra -> Expectation
shouldDropFor out = shouldDrop out . bech32Of

shouldPass :: HasCallStack => ServerOutput Tx -> Text -> Expectation
shouldPass out address = matches out address `shouldBe` True

shouldDrop :: HasCallStack => ServerOutput Tx -> Text -> Expectation
shouldDrop out address = matches out address `shouldBe` False

matches :: ServerOutput Tx -> Text -> Bool
matches output =
  txContainsAddr serverOutputFilter timed
 where
  timed = TimedServerOutput{seq = 0, time = posixSecondsToUTCTime 0, output}

bech32Of :: AddressInEra -> Text
bech32Of = \case
  ShelleyAddressInEra addr -> serialiseToBech32 addr
  ByronAddressInEra{} -> error "unexpected Byron address in fixture"
