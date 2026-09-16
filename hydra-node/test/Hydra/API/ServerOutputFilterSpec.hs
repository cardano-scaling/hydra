-- | Tests for the @?address=@ output filter applied by 'Hydra.API.WSServer'.
--
-- The filter decides what a client connected with an address sees. Getting it
-- wrong either leaks other parties' transactions or silently hides a client's
-- own, and neither shows up as an error anywhere. These are the filter's
-- semantics; 'Hydra.API.ServerSpec' covers the server-side wiring that reaches
-- them.
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
import Test.Hydra.Tx.Fixture (testHeadId, testNetworkId)
import Test.Hydra.Tx.Gen (genVerificationKey)

spec :: Spec
spec = parallel $ do
  describe "SnapshotConfirmed" $ do
    it "passes a snapshot whose confirmed tx pays the address" $
      snapshotConfirmed [txPayingTo [aliceAddress]] `shouldPassFor` aliceAddress

    it "drops a snapshot when no confirmed tx pays the address" $
      snapshotConfirmed [txPayingTo [bobAddress]] `shouldDropFor` aliceAddress

    -- A snapshot can confirm no transaction at all (it may only settle a
    -- deposit or a decommit). Such a snapshot carries no address, so an
    -- address-filtered client never hears about it.
    it "drops a snapshot confirming no transactions" $
      snapshotConfirmed [] `shouldDropFor` aliceAddress

    it "passes when any one of several confirmed txs pays the address" $
      snapshotConfirmed [txPayingTo [bobAddress], txPayingTo [carolAddress, aliceAddress]]
        `shouldPassFor` aliceAddress

    it "passes when the address is any one of a tx's outputs" $
      snapshotConfirmed [txPayingTo [bobAddress, carolAddress, aliceAddress]]
        `shouldPassFor` aliceAddress

  describe "address comparison" $ do
    -- The filter compares serialised addresses exactly, so anything that is
    -- not one of the outputs' own bech32 forms matches nothing at all.
    it "does not match a well-formed address that is not an output" $
      snapshotConfirmed [txPayingTo [aliceAddress, bobAddress]] `shouldDropFor` carolAddress

    it "does not match a string that is not an address" $ do
      snapshotConfirmed [txPayingTo [aliceAddress]] `shouldDrop` "invalid"
      snapshotConfirmed [txPayingTo [aliceAddress]] `shouldDrop` ""

    it "does not match an address truncated or extended by one character" $ do
      let addr = bech32Of aliceAddress
      snapshotConfirmed [txPayingTo [aliceAddress]] `shouldDrop` Text.init addr
      snapshotConfirmed [txPayingTo [aliceAddress]] `shouldDrop` (addr <> "x")

    -- The address a client puts in its query string comes from
    -- 'serialiseAddress' on an 'AddressInEra', while the filter derives the
    -- output's own text via 'serialiseToBech32' on the unwrapped Shelley
    -- address. They have to agree or every filtered client sees nothing.
    it "agrees with the serialiseAddress form a client would send" $
      snapshotConfirmed [txPayingTo [aliceAddress]] `shouldPass` serialiseAddress aliceAddress

  -- Everything that is not a 'SnapshotConfirmed' carries no transaction the
  -- filter could inspect, so it reaches the client regardless of the address.
  describe "other outputs" $
    for_ otherOutputs $ \(name, out) ->
      it ("passes " <> name) $
        out `shouldPassFor` carolAddress

-- * Fixtures

-- | Three distinct payment addresses on the test network.
aliceAddress, bobAddress, carolAddress :: AddressInEra
aliceAddress = addressFor 1
bobAddress = addressFor 2
carolAddress = addressFor 3

addressFor :: Int -> AddressInEra
addressFor seed = mkVkAddress testNetworkId (genVerificationKey `generateWith` seed)

-- | A transaction paying 2 Ada to each of the given addresses.
txPayingTo :: [AddressInEra] -> Tx
txPayingTo addresses =
  unsafeBuildTransaction $
    defaultTxBodyContent
      & addTxInsSpending [genTxIn `generateWith` 42]
      & addTxOuts
        [ TxOut address (lovelaceToValue 2_000_000) TxOutDatumNone ReferenceScriptNone
        | address <- addresses
        ]

snapshotConfirmed :: [Tx] -> ServerOutput Tx
snapshotConfirmed confirmed =
  SnapshotConfirmed
    { headId = testHeadId
    , snapshot = testSnapshot 1 0 confirmed mempty
    , signatures = mempty
    }

-- | A representative output per non-'SnapshotConfirmed' shape: one carrying no
-- transaction, one carrying a UTxO and one carrying a transaction of its own.
otherOutputs :: [(String, ServerOutput Tx)]
otherOutputs =
  [ ("HeadIsOpen", HeadIsOpen{headId = testHeadId, parties = []})
  , ("TxValid", TxValid{headId = testHeadId, transactionId = getTxId (getTxBody (txPayingTo [aliceAddress]))})
  , ("HeadIsFinalized", HeadIsFinalized{headId = testHeadId, finalizedUTxO = mempty})
  ,
    ( "DecommitRequested"
    , DecommitRequested
        { headId = testHeadId
        , decommitTx = txPayingTo [aliceAddress]
        , utxoToDecommit = mempty
        }
    )
  , ("NetworkConnected", NetworkConnected)
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

-- | The bech32 form of an address, as the filter derives it from a transaction
-- output.
bech32Of :: AddressInEra -> Text
bech32Of = \case
  ShelleyAddressInEra addr -> serialiseToBech32 addr
  ByronAddressInEra{} -> error "unexpected Byron address in fixture"
