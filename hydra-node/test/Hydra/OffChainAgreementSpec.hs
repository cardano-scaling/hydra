-- | Off-chain differential (real-node bindings): the Agda-extracted §6 handler decisions
-- ("Hydra.Agda.OffChainReference") checked against the REAL 'Hydra.HeadLogic.update' outcomes, not
-- against transcriptions of the figure. This extends 'Hydra.OffChainLeaderSpec' (which binds
-- @leaderRef@ to the real @isLeader@) to three more decisions, closing the figure↔Agda↔Haskell loop:
--
--   * @signEligibleRef@ vs @onOpenNetworkReqSn@: we drive 'update' with a @ReqSn@ over an open state
--     with version v̂ and a settled snapshot ŝ (confirmed = seen, nothing in flight) and binarise the
--     outcome: ACCEPT iff the node reaches the signing continuation ('SnapshotRequested' + an @AckSn@
--     effect). The node splits the figure's single @require@ into an Error (number:
--     'ReqSnNumberInvalid', leader: 'ReqSnNotLeader') and a Wait (version: 'WaitOnSnapshotVersion',
--     so a follower behind on the version bump does not drop the message); Error and Wait both map to
--     non-accept. The reference's leader input is resolved by the EXTRACTED @leaderRef@ (itself bound
--     to the real @isLeader@ in 'Hydra.OffChainLeaderSpec'), so the composed decision is fully Agda-derived.
--
--   * @reqDecEligibleRef@ vs @onOpenNetworkReqDec@: ACCEPT iff the node records the decommit
--     ('DecommitRecorded'). A pending deposit (commit in flight, @currentDepositTxId@ set) makes the
--     node WAIT ('WaitOnUnresolvedCommit'); an in-flight decommit makes it WAIT at ttl > 0
--     ('WaitOnNotApplicableDecommitTx' with 'DecommitAlreadyInFlight'); both are non-accept, matching
--     the reference's single Bool.
--
--   * @depositStatusRef@ vs the deposit-status transition the node applies on a chain 'Tick'
--     ('onOpenChainTick' via @determineNextDepositStatus@): starting from a fresh Inactive deposit,
--     one tick at time t yields 'DepositExpired' / 'DepositActivated' / no status event, mapping to
--     ExpiredS / ActiveS / InactiveS.
--
-- Domain note (deposit status): the extracted decision uses Nat truncated subtraction for
-- @deadline − T_deposit@ whereas the node subtracts over 'UTCTime'; the two agree whenever
-- @deadline ≥ T_deposit@, which the protocol guarantees (an observed deposit deadline sits a full
-- deposit period after creation). The property therefore quantifies over that domain, with all times
-- as whole POSIX seconds (exact in both representations).
module Hydra.OffChainAgreementSpec (spec) where

import Hydra.Prelude
import Test.Hydra.Prelude

import Data.Map.Strict qualified as Map
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.API.ServerOutput (DecommitInvalidReason (..))
import Hydra.Agda.OffChainReference (
  HsDepositStatus (..),
  depositStatusRef,
  leaderRef,
  reqDecEligibleRef,
  signEligibleRef,
 )
import Hydra.Chain (ChainEvent (..))
import Hydra.HeadLogic (
  CoordinatedHeadState (..),
  Input (..),
  LogicError (..),
  Outcome (..),
  RequirementFailure (..),
  SeenSnapshot (..),
  StateChanged (..),
  WaitReason (..),
  update,
 )
import Hydra.HeadLogicSpec (assertWait, inOpenState, inOpenState', receiveMessageFrom, testSnapshot)
import Hydra.Ledger.Simple (SimpleTx (..), simpleLedger, utxoRef)
import Hydra.Network.Message (Message (..))
import Hydra.Node.Environment (Environment (..))
import Hydra.Node.State (Deposit (..), DepositStatus (..), NodeState (..))
import Hydra.Options (defaultContestationPeriod, defaultDepositPeriod, defaultUnsyncedPeriod)
import Hydra.Prelude qualified as Prelude
import Hydra.Tx.Crypto (aggregate)
import Hydra.Tx.Party (Party)
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..))
import Test.Hydra.Tx.Fixture (alice, aliceSk, bob, carol, deriveOnChainId, testHeadId)
import Test.QuickCheck (choose, elements, forAll, (===))

threeParties :: [Party]
threeParties = [alice, bob, carol]

-- The node under test is alice; the handlers' guards concern the SENDER, so one env suffices.
aliceEnv :: Environment
aliceEnv =
  Environment
    { party = alice
    , signingKey = aliceSk
    , otherParties = [bob, carol]
    , contestationPeriod = defaultContestationPeriod
    , depositPeriod = defaultDepositPeriod
    , unsyncedPeriod = defaultUnsyncedPeriod
    , participants = deriveOnChainId <$> threeParties
    , configuredPeers = ""
    }

-- 'update' ignores the wall clock on the NetworkInput path; any fixed time works.
time0 :: UTCTime
time0 = posixSecondsToUTCTime 0

posixTime :: Integer -> UTCTime
posixTime = posixSecondsToUTCTime . fromInteger

-- ── reqSn signing eligibility ────────────────────────────────────────────────────────────────────────

-- Open state with version v̂ and a settled snapshot ŝ: confirmed = seen (nothing in flight), so the
-- handler's wait-guards outside the modeled decision (ŝ = S̄.s) hold by construction.
reqSnState :: Integer -> Integer -> NodeState SimpleTx
reqSnState vHat sHat =
  inOpenState' threeParties $
    CoordinatedHeadState
      { localUTxO = mempty
      , allTxs = mempty
      , localTxs = mempty
      , confirmedSnapshot =
          if sHat == 0
            then InitialSnapshot testHeadId
            else ConfirmedSnapshot (testSnapshot (fromInteger sHat) (fromInteger vHat) [] mempty) (aggregate [])
      , seenSnapshot = if sHat == 0 then NoSeenSnapshot else LastSeenSnapshot (fromInteger sHat)
      , currentDepositTxId = Nothing
      , decommitTx = Nothing
      , version = fromInteger vHat
      }

-- Run the REAL handler on a (v, s) request from the given sender.
reqSnOutcome :: Integer -> Integer -> Integer -> Integer -> Party -> Outcome SimpleTx
reqSnOutcome vHat sHat v s sender =
  update aliceEnv simpleLedger time0 (reqSnState vHat sHat) $
    receiveMessageFrom sender (ReqSn (fromInteger v) (fromInteger s) [] Nothing Nothing)

-- ACCEPT iff the node reaches the signing continuation; Error and Wait are both non-accept.
reqSnAccepts :: Outcome SimpleTx -> Bool
reqSnAccepts = \case
  Continue{stateChanges} -> any isRequested stateChanges
  _ -> False
 where
  isRequested :: StateChanged SimpleTx -> Bool
  isRequested = \case
    SnapshotRequested{} -> True
    _ -> False

-- ── reqDec eligibility ───────────────────────────────────────────────────────────────────────────────

-- The requested decommit (no inputs, so it applies to any local UTxO) and a distinct in-flight one.
requestedDecommit :: SimpleTx
requestedDecommit = SimpleTx 1 mempty (utxoRef 1)

inFlightDecommit :: SimpleTx
inFlightDecommit = SimpleTx 2 mempty (utxoRef 2)

-- Open state on the reqDec decision's domain: a pending deposit (U_α ≠ ∅) and/or an in-flight
-- decommit (tx_ω ≠ ⊥).
reqDecState :: Bool -> Bool -> NodeState SimpleTx
reqDecState commitInFlight decommitInFlight =
  inOpenState' threeParties $
    CoordinatedHeadState
      { localUTxO = mempty
      , allTxs = mempty
      , localTxs = mempty
      , confirmedSnapshot = InitialSnapshot testHeadId
      , seenSnapshot = NoSeenSnapshot
      , currentDepositTxId = if commitInFlight then Just 7 else Nothing
      , decommitTx = if decommitInFlight then Just inFlightDecommit else Nothing
      , version = 0
      }

reqDecOutcome :: Bool -> Bool -> Outcome SimpleTx
reqDecOutcome commitInFlight decommitInFlight =
  update aliceEnv simpleLedger time0 (reqDecState commitInFlight decommitInFlight) $
    receiveMessageFrom bob ReqDec{transaction = requestedDecommit}

reqDecAccepts :: Outcome SimpleTx -> Bool
reqDecAccepts = \case
  Continue{stateChanges} -> any isRecorded stateChanges
  _ -> False
 where
  isRecorded :: StateChanged SimpleTx -> Bool
  isRecorded = \case
    DecommitRecorded{} -> True
    _ -> False

-- ── deposit status on tick ───────────────────────────────────────────────────────────────────────────

-- An env whose deposit period is the modeled T_deposit (whole seconds).
tickEnv :: Integer -> Environment
tickEnv tDep = aliceEnv{depositPeriod = fromInteger tDep}

-- Open state holding one FRESH (Inactive) pending deposit with the given creation time and deadline.
depositState :: Integer -> Integer -> NodeState SimpleTx
depositState created deadline =
  (inOpenState threeParties)
    { pendingDeposits =
        Map.fromList
          [
            ( 1
            , Deposit
                { headId = testHeadId
                , deposited = utxoRef 1
                , created = posixTime created
                , deadline = posixTime deadline
                , status = Inactive
                }
            )
          ]
    }

-- One REAL tick at time t (now = chainTime, so the node stays in sync) over the fresh deposit; the
-- resulting status is read off the emitted transition event (none = still Inactive).
realTickStatus :: Integer -> Integer -> Integer -> Integer -> HsDepositStatus
realTickStatus created deadline tDep t =
  case update (tickEnv tDep) simpleLedger (posixTime t) (depositState created deadline) tick of
    Continue{stateChanges}
      | any isExpired stateChanges -> ExpiredS
      | any isActivated stateChanges -> ActiveS
      | otherwise -> InactiveS
    outcome -> Prelude.error $ "tick produced a non-Continue outcome: " <> show outcome
 where
  tick = ChainInput Tick{chainTime = posixTime t, chainPoint = 1}

  isExpired :: StateChanged SimpleTx -> Bool
  isExpired = \case
    DepositExpired{} -> True
    _ -> False

  isActivated :: StateChanged SimpleTx -> Bool
  isActivated = \case
    DepositActivated{} -> True
    _ -> False

spec :: Spec
spec = parallel $ do
  describe "reqSn signing eligibility: extracted signEligibleRef vs the real onOpenNetworkReqSn" $ do
    it "anchor: an eligible ReqSn (v = v̂, s = ŝ + 1, sender leads s) is signed by the real node" $ do
      signEligibleRef 0 0 1 0 (leaderRef 2 1 0) `shouldBe` True
      reqSnAccepts (reqSnOutcome 0 0 0 1 alice) `shouldBe` True
    it "a wrong snapshot number is the node's ReqSnNumberInvalid" $
      reqSnOutcome 0 0 0 2 alice `shouldBe` Error (RequireFailed $ ReqSnNumberInvalid 2 0)
    it "a non-leader sender is the node's ReqSnNotLeader" $
      reqSnOutcome 0 0 0 1 bob `shouldBe` Error (RequireFailed $ ReqSnNotLeader 1 bob)
    it "a version mismatch WAITS (WaitOnSnapshotVersion), which is non-accept" $
      reqSnOutcome 0 0 1 1 alice `assertWait` WaitOnSnapshotVersion 1
    prop "signEligibleRef === real ReqSn accept/reject across (v, v̂, s, ŝ, sender)" $
      forAll (choose (0, 1)) $ \vHat ->
        forAll (choose (0, 2)) $ \v ->
          forAll (choose (0, 2)) $ \sHat ->
            forAll (choose (0, 4)) $ \s ->
              forAll (elements (zip [0 ..] threeParties)) $ \(i, sender) ->
                signEligibleRef v vHat s sHat (leaderRef 2 s i)
                  === reqSnAccepts (reqSnOutcome vHat sHat v s sender)

  describe "reqDec eligibility: extracted reqDecEligibleRef vs the real onOpenNetworkReqDec" $ do
    it "anchor: with nothing in flight the real node records the decommit" $ do
      reqDecEligibleRef False False `shouldBe` True
      reqDecAccepts (reqDecOutcome False False) `shouldBe` True
    it "a pending deposit (commit in flight) makes the real node WAIT (WaitOnUnresolvedCommit)" $
      reqDecOutcome True False `assertWait` WaitOnUnresolvedCommit{commitUTxO = mempty}
    it "an in-flight decommit makes the real node WAIT (DecommitAlreadyInFlight)" $
      reqDecOutcome False True
        `assertWait` WaitOnNotApplicableDecommitTx
          { notApplicableReason = DecommitAlreadyInFlight{otherDecommitTxId = 2}
          }
    prop "reqDecEligibleRef === real ReqDec accept/non-accept across (commit?, decommit?)" $
      \commitInFlight decommitInFlight ->
        reqDecEligibleRef commitInFlight decommitInFlight
          === reqDecAccepts (reqDecOutcome commitInFlight decommitInFlight)

  describe "deposit status on tick: extracted depositStatusRef vs the real deposit transition" $ do
    -- The same boundary points the extracted checker pins, now against the real node.
    it "Inactive before created + T_deposit (both)" $ do
      depositStatusRef 0 100 10 5 `shouldBe` InactiveS
      realTickStatus 0 100 10 5 `shouldBe` InactiveS
    it "still Inactive AT created + T_deposit (strictly-greater boundary, both)" $ do
      depositStatusRef 0 100 10 10 `shouldBe` InactiveS
      realTickStatus 0 100 10 10 `shouldBe` InactiveS
    it "Active once t > created + T_deposit (both)" $ do
      depositStatusRef 0 100 10 15 `shouldBe` ActiveS
      realTickStatus 0 100 10 15 `shouldBe` ActiveS
    it "still Active AT deadline − T_deposit (not yet expired, both)" $ do
      depositStatusRef 0 100 10 90 `shouldBe` ActiveS
      realTickStatus 0 100 10 90 `shouldBe` ActiveS
    it "Expired once t > deadline − T_deposit (both)" $ do
      depositStatusRef 0 100 10 95 `shouldBe` ExpiredS
      realTickStatus 0 100 10 95 `shouldBe` ExpiredS
    prop "depositStatusRef === real tick status transition (time grid incl. boundaries)" $
      forAll (choose (0, 3)) $ \created ->
        forAll (choose (1, 3)) $ \tDep ->
          forAll (choose (0, 4)) $ \slack ->
            let deadline = created + 2 * tDep + slack
             in forAll (choose (0, deadline + tDep + 1)) $ \t ->
                  depositStatusRef created deadline tDep t
                    === realTickStatus created deadline tDep t
