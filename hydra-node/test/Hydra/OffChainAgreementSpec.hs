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
--   * @notAlreadySignedRef@ vs @onOpenNetworkAckSn@'s @requireNotSignedYet@: over a directly
--     constructed in-flight 'SeenSnapshot' whose signatories map holds REAL signatures, an @AckSn@
--     from a sender already in the map is the node's 'SnapshotAlreadySigned'; a fresh sender is not.
--
--   * @allSignedRef@ vs the round completion of @onOpenNetworkAckSn@: a fresh sender's ack CONFIRMS
--     (the node aggregates the real signatures in order and verifies the real multisignature before
--     emitting 'SnapshotConfirmed') exactly when it completes the n-of-n signer set; the composed
--     decision is @notAlreadySignedRef ∧ allSignedRef (sender ∷ Σ̂)@.
--
--   * @contestEligibleRef@ vs @onOpenChainCloseTx@: observing a close of snapshot s_c while our
--     confirmed snapshot is S̄.s posts a 'ContestTx' exactly when S̄.s > s_c.
--
-- Domain note (deposit status): the extracted decision uses Nat truncated subtraction for
-- @deadline − T_deposit@ whereas the node subtracts over 'UTCTime'; the two agree whenever
-- @deadline ≥ T_deposit@, which the protocol guarantees (an observed deposit deadline sits a full
-- deposit period after creation). The property therefore quantifies over that domain, with all times
-- as whole POSIX seconds (exact in both representations).
--
-- Scope note (ackSn): the extracted guards model the COUNTING decisions (who signed, n-of-n); the
-- collected signatures themselves are held healthy, exactly as the on-chain reference holds crypto
-- conjuncts healthy. The real node additionally verifies the aggregate multisignature at
-- confirmation; a corrupt collected signature makes it reject ('InvalidMultisignature') where the
-- counting reference alone would accept, demonstrated as a validator-only rejection below.
module Hydra.OffChainAgreementSpec (spec) where

import Hydra.Prelude
import Test.Hydra.Prelude

import Data.Map.Strict qualified as Map
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.API.ServerOutput (DecommitInvalidReason (..))
import Hydra.Agda.OffChainReference (
  HsDepositStatus (..),
  allSignedRef,
  contestEligibleRef,
  depositStatusRef,
  leaderRef,
  notAlreadySignedRef,
  reqDecEligibleRef,
  signEligibleRef,
 )
import Hydra.Chain (ChainEvent (..), OnChainTx (..), PostChainTx (..))
import Hydra.HeadLogic (
  CoordinatedHeadState (..),
  Effect (..),
  Input (..),
  LogicError (..),
  Outcome (..),
  RequirementFailure (..),
  SeenSnapshot (..),
  StateChanged (..),
  WaitReason (..),
  mkSeenSnapshot,
  update,
 )
import Hydra.HeadLogicSpec (assertWait, inOpenState, inOpenState', observeTx, receiveMessageFrom, testSnapshot)
import Hydra.Ledger.Simple (SimpleTx (..), simpleLedger, utxoRef)
import Hydra.Network.Message (Message (..))
import Hydra.Node.Environment (Environment (..))
import Hydra.Node.State (Deposit (..), DepositStatus (..), NodeState (..))
import Hydra.Options (defaultContestationPeriod, defaultDepositPeriod, defaultUnsyncedPeriod)
import Hydra.Prelude qualified as Prelude
import Hydra.Tx.Crypto (HydraKey, Signature, SigningKey, aggregate, sign)
import Hydra.Tx.Party (Party)
import Hydra.Tx.Secret (Secret)
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot)
import Test.Hydra.Tx.Fixture (alice, aliceSk, bob, bobSk, carol, carolSk, deriveOnChainId, testHeadId)
import Test.QuickCheck (choose, elements, forAll, sublistOf, (===))

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

-- ── ackSn collect/confirm ────────────────────────────────────────────────────────────────────────────

-- Index ↔ party ↔ signing key, positional in 'threeParties' (as everywhere in this module).
partySks :: [(Party, Secret (SigningKey HydraKey))]
partySks = [(alice, aliceSk), (bob, bobSk), (carol, carolSk)]

partySkAt :: Integer -> (Party, Secret (SigningKey HydraKey))
partySkAt i = case drop (fromInteger i) partySks of
  x : _ -> x
  [] -> Prelude.error "partySkAt: index out of range"

-- The in-flight round's snapshot (ŝ = 1 on v̂ = 0) and its real per-party signatures.
ackSnapshot :: Snapshot SimpleTx
ackSnapshot = testSnapshot 1 0 [] mempty

ackSigFor :: Integer -> (Party, Signature (Snapshot SimpleTx))
ackSigFor i = let (p, sk) = partySkAt i in (p, sign sk ackSnapshot)

-- Open state mid-round: the seen snapshot is in flight with the given parties' REAL signatures
-- already collected ('mkSeenSnapshot' caches the signable bytes the verification runs over).
ackState :: [(Party, Signature (Snapshot SimpleTx))] -> NodeState SimpleTx
ackState collected =
  inOpenState' threeParties $
    CoordinatedHeadState
      { localUTxO = mempty
      , allTxs = mempty
      , localTxs = mempty
      , confirmedSnapshot = InitialSnapshot testHeadId
      , seenSnapshot = mkSeenSnapshot ackSnapshot (Map.fromList collected)
      , currentDepositTxId = Nothing
      , decommitTx = Nothing
      , version = 0
      }

-- Run the REAL handler on sender's (real-signature) AckSn over the given collected subset.
ackOutcome :: [Integer] -> Integer -> Outcome SimpleTx
ackOutcome signedIdxs senderIdx =
  update aliceEnv simpleLedger time0 (ackState (ackSigFor <$> signedIdxs)) $
    receiveMessageFrom sender (AckSn senderSig 1)
 where
  (sender, _) = partySkAt senderIdx
  (_, senderSig) = ackSigFor senderIdx

-- The already-signed reject direction: the node's SnapshotAlreadySigned require failure.
ackAlreadySigned :: Outcome SimpleTx -> Bool
ackAlreadySigned = \case
  Error (RequireFailed SnapshotAlreadySigned{}) -> True
  _ -> False

-- The round completes: SnapshotConfirmed is emitted (behind the real multisignature verification).
ackConfirms :: Outcome SimpleTx -> Bool
ackConfirms = \case
  Continue{stateChanges} -> any isConfirmed stateChanges
  _ -> False
 where
  isConfirmed :: StateChanged SimpleTx -> Bool
  isConfirmed = \case
    SnapshotConfirmed{} -> True
    _ -> False

-- Validator-only rejection (the crypto boundary): alice's COLLECTED signature is over garbage, and
-- carol's final ack completes the signer set, so the counting guards pass but the real aggregate
-- verification fails.
ackOutcomeCorrupt :: Outcome SimpleTx
ackOutcomeCorrupt =
  update aliceEnv simpleLedger time0 (ackState [corrupt, ackSigFor 1]) $
    receiveMessageFrom sender (AckSn senderSig 1)
 where
  corrupt = (alice, coerce (sign aliceSk ("garbage" :: ByteString)))
  (sender, _) = partySkAt 2
  (_, senderSig) = ackSigFor 2

ackInvalidMultisig :: Outcome SimpleTx -> Bool
ackInvalidMultisig = \case
  Error (RequireFailed InvalidMultisignature{}) -> True
  _ -> False

-- ── contest eligibility on a close observation ───────────────────────────────────────────────────────

-- Observe a close of snapshot s_c while our confirmed snapshot is S̄.s ('reqSnState' already builds
-- exactly the open state with a confirmed snapshot at a given number; version 0 here).
contestOutcome :: Integer -> Integer -> Outcome SimpleTx
contestOutcome sBar sc =
  update aliceEnv simpleLedger time0 (reqSnState 0 sBar) $
    observeTx
      OnCloseTx
        { headId = testHeadId
        , snapshotNumber = fromInteger sc
        , contestationDeadline = posixTime 1_000
        }

contestPosts :: Outcome SimpleTx -> Bool
contestPosts = \case
  Continue{effects} -> any isContest effects
  _ -> False
 where
  isContest :: Effect SimpleTx -> Bool
  isContest = \case
    OnChainEffect{postChainTx = ContestTx{}} -> True
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

  describe "ackSn collect/confirm: extracted notAlreadySignedRef/allSignedRef vs the real onOpenNetworkAckSn" $ do
    it "anchor: a fresh, non-final ack is collected (neither already-signed nor confirmed)" $ do
      notAlreadySignedRef [0] 1 `shouldBe` True
      allSignedRef 3 [1, 0] `shouldBe` False
      ackAlreadySigned (ackOutcome [0] 1) `shouldBe` False
      ackConfirms (ackOutcome [0] 1) `shouldBe` False
    it "anchor: the final ack CONFIRMS (real signatures aggregated in order and verified)" $ do
      allSignedRef 3 [2, 0, 1] `shouldBe` True
      ackConfirms (ackOutcome [0, 1] 2) `shouldBe` True
    it "a duplicate ack is the node's SnapshotAlreadySigned" $ do
      notAlreadySignedRef [0, 1] 1 `shouldBe` False
      ackAlreadySigned (ackOutcome [0, 1] 1) `shouldBe` True
    prop "notAlreadySignedRef === real not-already-signed across (signed subset, sender)" $
      forAll (sublistOf [0, 1, 2]) $ \signedIdxs ->
        forAll (elements [0, 1, 2]) $ \senderIdx ->
          notAlreadySignedRef signedIdxs senderIdx
            === not (ackAlreadySigned (ackOutcome signedIdxs senderIdx))
    prop "composed: the ack CONFIRMS iff the sender is fresh AND completes the n-of-n signer set" $
      forAll (sublistOf [0, 1, 2]) $ \signedIdxs ->
        forAll (elements [0, 1, 2]) $ \senderIdx ->
          (notAlreadySignedRef signedIdxs senderIdx && allSignedRef 3 (senderIdx : signedIdxs))
            === ackConfirms (ackOutcome signedIdxs senderIdx)
    it "scope: a corrupt collected signature fails the real multisignature verification (the counting reference alone would accept)" $ do
      allSignedRef 3 [2, 0, 1] `shouldBe` True
      ackInvalidMultisig ackOutcomeCorrupt `shouldBe` True

  describe "contest eligibility: extracted contestEligibleRef vs the real onOpenChainCloseTx" $ do
    it "anchor: a newer confirmed snapshot posts a ContestTx" $ do
      contestEligibleRef 1 0 `shouldBe` True
      contestPosts (contestOutcome 1 0) `shouldBe` True
    it "an equal snapshot number does NOT contest (strictly-greater boundary)" $ do
      contestEligibleRef 1 1 `shouldBe` False
      contestPosts (contestOutcome 1 1) `shouldBe` False
    prop "contestEligibleRef === real contest re-post across (S̄.s, s_c)" $
      forAll (choose (0, 2)) $ \sBar ->
        forAll (choose (0, 3)) $ \sc ->
          contestEligibleRef sBar sc === contestPosts (contestOutcome sBar sc)
