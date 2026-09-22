{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A /Model/ of the Hydra head Protocol.
--
-- This model integrates in a single state-machine like abstraction the whole behaviour of
-- a Hydra Head, taking into account both on-chain state and contracts, and off-chain
-- interactions. It is written from the point of view of a pre-defined set of Hydra node
-- /operators/ that want to create a channel between them.
-- It's a "happy path" model that does not implement any kind of adversarial behaviour and
-- whose transactions are very simple: Each tx is a payment of one Ada-only UTxO transferred
-- to another party in full, without any change.
--
-- More intricate and specialised models shall be developed once we get a firmer grasp of
-- the whole framework, injecting faults, taking into account more parts of the stack,
-- modelling more complex transactions schemes...
module Hydra.Model where

import Hydra.Cardano.Api hiding (CardanoSigningKey (..), getVerificationKey, utxoFromTx)
import Hydra.Prelude hiding (Any, label, lookup, toList)
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Binary (serialize', unsafeDeserialize')
import Control.Concurrent.Class.MonadSTM (
  modifyTVar,
  readTVarIO,
  retry,
 )
import Control.Monad.Class.MonadAsync (cancel, link)
import Control.Tracer.JSON (Tracer)
import Data.EventSource.Rotation (EventStore)
import Data.List (nub, (\\))
import Data.List qualified as List
import Data.Map.Strict ((!))
import Data.Map.Strict qualified as Map
import Data.Secret (Secret, mkSecret)
import Data.Set qualified as Set
import GHC.IsList (IsList (..))
import GHC.Natural (wordToNatural)
import Hydra.API.ClientInput (ClientInput)
import Hydra.API.ClientInput qualified as Input
import Hydra.API.ServerOutput (ServerOutput (..))
import Hydra.BehaviorSpec (
  RequeueMode (..),
  SimulatedChainNetwork (..),
  TestHydraClient (..),
  createHydraNodeWithEventStore,
  createTestHydraClient,
  getHeadUTxO,
  shortLabel,
  waitUntilMatch,
 )
import Hydra.Chain (maximumNumberOfParties)
import Hydra.Chain.Direct.State (initialChainState)
import Hydra.HeadLogic.State qualified as HeadLogic
import Hydra.HeadLogic.StateEvent (StateEvent)
import Hydra.Ledger.Cardano (cardanoLedger, mkSimpleTx)
import Hydra.Logging.Messages (HydraLog (DirectChain, Node))
import Hydra.Model.MockChain (FaultMode (..), mockChainAndNetwork)
import Hydra.Model.Payment (CardanoSigningKey (..), Payment (..), applyTx, genAdaValue)
import Hydra.Node (HydraNode (..), NodeStateHandler (..), runHydraNode)
import Hydra.Node.State (NodeState (..))
import Hydra.NodeSpec (createMockEventStoreWithReader)
import Hydra.Options (defaultContestationPeriod, defaultDepositPeriod)
import Hydra.Tx (HeadId)
import Hydra.Tx.ContestationPeriod (ContestationPeriod (..))
import Hydra.Tx.Crypto (HydraKey, getVerificationKey)
import Hydra.Tx.DepositPeriod (DepositPeriod (..))
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.IsTx (IsTx (..))
import Hydra.Tx.Party (Party (..), deriveParty)
import Hydra.Tx.Snapshot qualified as Snapshot
import Test.Hydra.Node.Fixture (defaultGlobals, defaultLedgerEnv, testNetworkId)
import Test.Hydra.Tx.Gen (genSigningKey)
import Test.QuickCheck (choose, chooseEnum, discard, elements, frequency, listOf, resize, sized, suchThat, tabulate, vectorOf)
import Test.QuickCheck.DynamicLogic (DynLogicModel)
import Test.QuickCheck.StateModel (Any (..), HasVariables, PostconditionM, Realized, RunModel (..), StateModel (..), Var, VarContext, counterexamplePost)
import Test.QuickCheck.StateModel.Variables (HasVariables (..))
import Prelude qualified

-- * The Model

-- | State maintained by the model.
data WorldState = WorldState
  { hydraParties :: [(Secret (SigningKey HydraKey), CardanoSigningKey)]
  -- ^ List of parties identified by both signing keys required to run protocol.
  -- This list must not contain any duplicated key.
  , hydraState :: GlobalState
  -- ^ Expected consensus state
  -- All nodes should be in the same state.
  , availableToDeposit :: UTxOType Payment
  -- ^ UTxO available to be committed incrementally, seeded from
  -- 'additionalUTxO' at 'Seed'. NOTE: We must not add UTxO we decommitted to
  -- this as the 'Payment' transaction model results in non-unique transaction
  -- ids when running the model. For the same reason a random 'Deposit' always
  -- commits /all/ of one signer's available UTxO at once ('toRealUTxO'
  -- assigns mocked TxIns per signer starting from index 0, so two separate
  -- deposits by the same signer would collide).
  , pendingCommits :: [(Var TxId, UTxOType Payment)]
  -- ^ Deposits submitted via 'SubmitDeposit' and recorded on chain, but not
  -- yet observed as finalized ('ObserveCommitFinalized'). Several can be in
  -- flight at once, which is what lets a fork erase one settlement while
  -- another is pending.
  , pendingDecommits :: [(Var UTxO, Payment)]
  -- ^ Decommits submitted via 'SubmitDecommit' whose snapshot is confirmed
  -- but whose decrement is not yet observed as finalized
  -- ('ObserveDecommitFinalized').
  , settledCommits :: [Var TxId]
  -- ^ Commits already observed as finalized, once per observation. They may
  -- be observed again: a fork that erases the increment makes it re-land
  -- (re-posted by the nodes or re-included from the mempool), which the nodes
  -- report as a second 'CommitFinalized'; the n-th observation waits for the
  -- n-th report.
  , settledDecommits :: [Var UTxO]
  -- ^ Decommits already observed as finalized, once per observation; see
  -- 'settledCommits'.
  , concurrentSettlements :: Bool
  -- ^ Generator mode, set by 'Seed'.
  }
  deriving stock (Eq, Show)

-- | Global state of the Head protocol.
-- While each participant in the Hydra Head protocol has its own private
-- view of the state, we model the expected global state whose properties
-- stem from the consensus built into the Head protocol. In other words, this
-- state is what each node's local state should be /eventually/.
data GlobalState
  = -- | Start of the "world".
    --  This state is left implicit in the node's logic as it
    --  represents that state where the node does not even
    --  exist.
    Start
  | Idle
      { idleParties :: [Party]
      , cardanoKeys :: [VerificationKey PaymentKey]
      , contestationPeriod :: ContestationPeriod
      }
  | Open
      { headIdVar :: Var HeadId
      , headParameters :: HeadParameters
      , offChainState :: OffChainState
      , -- TODO: keep a single UTxOType Payment instead?
        committed :: Map Party (UTxOType Payment)
      , onChainVersion :: Natural
      -- ^ Expected open state version on chain: bumped by every settled
      -- increment ('Deposit') and decrement ('Decommit').
      }
  | Closed
      { headParameters :: HeadParameters
      , closedUTxO :: UTxOType Payment
      , unsettledAtClose :: UTxOType Payment
      -- ^ Outputs of settlements still pending when the head was closed: a
      -- pending commit's deposit, or a pending decommit's payout. Whether the
      -- settlement landed before the close is a race the model does not
      -- track, so each of these may or may not be part of the fanout.
      , fanoutDriving :: FanoutDriving
      -- ^ How the fanout is being driven, if it has started.
      , fannedOut :: UTxOType Payment
      -- ^ Outputs already handed to 'PartialFanoutStep' in manual mode.
      }
  | Final
      { finalUTxO :: UTxOType Payment
      , unsettledAtFinal :: UTxOType Payment
      -- ^ See 'unsettledAtClose'.
      }
  deriving stock (Eq, Show)

newtype OffChainState = OffChainState {confirmedUTxO :: UTxOType Payment}
  deriving stock (Eq, Show)

-- | How a closed head's fanout is driven. A plain 'Fanout' drains the head
-- automatically, possibly in several steps; 'PartialFanoutStep' hands the node
-- one selection at a time and the node only drains what it was given. The two
-- cannot be mixed: once a partial fanout started, 'Fanout' is rejected.
data FanoutDriving
  = FanoutNotStarted
  | FanoutAutoDraining
  | FanoutManual
  deriving stock (Eq, Show)

-- This is needed to be able to use `WorldState` inside DL formulae
instance DynLogicModel WorldState

-- | Basic instantiation of `StateModel` for our `WorldState` state.
instance StateModel WorldState where
  -- The list of possible "Actions" within our `Model`
  -- Not all of them need to actually represent an actual user `Action`, but they
  -- can represent _observations_ which are useful when defining properties in
  -- DL. Those observations would usually not be generated.
  data Action WorldState a where
    Seed ::
      { seedKeys :: [(Secret (SigningKey HydraKey), CardanoSigningKey)]
      , contestationPeriod :: ContestationPeriod
      , additionalUTxO :: UTxOType Payment
      , concurrentSettlements :: Bool
      , faultyParty :: Maybe (Party, FaultMode)
      -- \^ A party that misbehaves on the network, and how, see 'FaultMode'.
      } ->
      -- \^ Whether the random walk may have several deposits/decommits in
      -- flight at once ('SubmitDeposit' & co., forks in every 'RequeueMode')
      -- or settles each one before the next ('Deposit'/'Decommit', forks
      -- re-landing everything). See 'genOpenActions'.
      --
      -- TODO: Remove this switch and always settle concurrently. The
      -- sequential walk only exists because the concurrent one still finds
      -- open bugs (see the pending properties in 'Hydra.ModelSpec'); once
      -- those are fixed, every property should hold under concurrent
      -- settlements.

      Action WorldState ()
    Init :: Party -> Action WorldState HeadId
    Deposit :: {headIdVar :: Var HeadId, utxoToDeposit :: UTxOType Payment} -> Action WorldState ()
    Decommit :: {party :: Party, decommitTx :: Payment} -> Action WorldState ()
    -- Non-blocking variants of 'Deposit' and 'Decommit': submit and wait only
    -- until the deposit is recorded on chain (resp. the decommit's snapshot is
    -- confirmed), then observe settlement separately. This is what allows
    -- several settlements to be in flight when a fork hits.
    -- NOTE: No records possible here, see 'Fanout'.
    SubmitDeposit :: Var HeadId -> UTxOType Payment -> Action WorldState TxId
    -- Wait until the snapshot claiming the deposit is confirmed, i.e. its
    -- increment is in flight. Observation only, used by scripted scenarios.
    ObserveCommitApproved :: Var TxId -> Action WorldState ()
    ObserveCommitFinalized :: Var TxId -> Action WorldState ()
    -- Returns the decommitted UTxO as built on L2, to match the decrement's
    -- distributed outputs exactly in 'ObserveDecommitFinalized'.
    SubmitDecommit :: Party -> Payment -> Action WorldState UTxO
    ObserveDecommitFinalized :: Var UTxO -> Action WorldState ()
    Close :: {party :: Party} -> Action WorldState ()
    -- NOTE: No records possible here as we would duplicate 'Party' fields with
    -- different return values.
    Fanout :: Party -> Action WorldState UTxO
    -- Non-blocking fanout, in steps: start draining automatically, or hand
    -- the node one selection to distribute (manual mode), observe partial
    -- steps landing, and finally observe the head being finalized. Lets a
    -- fork hit while the fanout is in progress. Used by scripted scenarios.
    StartFanout :: Party -> Action WorldState ()
    PartialFanoutStep :: Party -> UTxOType Payment -> Action WorldState ()
    ObservePartialFanoutSteps :: Int -> Action WorldState ()
    ObserveFanoutFinalized :: Party -> Action WorldState UTxO
    NewTx :: Party -> Payment -> Action WorldState Payment
    Wait :: DiffTime -> Action WorldState ()
    ObserveConfirmedTx :: Var Payment -> Action WorldState ()
    -- Check that all parties have observed the head as open
    ObserveHeadIsOpen :: Action WorldState ()
    RollbackAndForward :: Natural -> Action WorldState ()
    -- Rollback onto a divergent fork: the rolled back blocks are dropped (not
    -- re-served); 'requeueErased' says which of their transactions are
    -- re-submitted (mempool re-inclusion), the rest only land if the nodes
    -- re-post them.
    RollbackAndFork :: {numberOfBlocks :: Natural, requeueErased :: RequeueMode} -> Action WorldState ()
    -- Crash a node (in-flight inputs are lost) and restart it from its event
    -- store, re-syncing the chain from genesis.
    RestartNode :: Party -> Action WorldState ()
    CloseWithInitialSnapshot :: Party -> Action WorldState ()
    StopTheWorld :: Action WorldState ()

  initialState =
    WorldState
      { hydraParties = mempty
      , hydraState = Start
      , availableToDeposit = mempty
      , pendingCommits = mempty
      , pendingDecommits = mempty
      , settledCommits = mempty
      , settledDecommits = mempty
      , concurrentSettlements = False
      }

  arbitraryAction :: VarContext -> WorldState -> Gen (Any (Action WorldState))
  arbitraryAction _ st@WorldState{hydraParties, hydraState, availableToDeposit, pendingCommits, pendingDecommits, concurrentSettlements} =
    case hydraState of
      Start -> Some <$> genSeed
      Idle{} -> Some <$> genInit hydraParties
      Open{headIdVar, offChainState = OffChainState{confirmedUTxO}} ->
        genOpenActions headIdVar confirmedUTxO
      Closed{} ->
        frequency
          [ (5, genFanout)
          , (1, genRollbackAndForward)
          ]
      Final{} -> Some <$> genSeed
   where
    -- NOTE: Some actions depend on confirmed 'UTxO' in the head so
    -- we need to make sure there are funds to spend when generating a
    -- `NewTx` action for example but also want to make sure that after
    -- a 'Decommit' we are not left without any funds so further actions
    -- can be generated.
    genOpenActions headIdVar confirmedUTxO =
      frequency $
        [ (1, genClose)
        , (1, genRollbackAndForward)
        , (1, genRollbackAndFork)
        ]
          -- 'RestartNode' models fail-recovery under load, see 'restartNodeEnabled'.
          <> [(1, genRestartNode) | restartNodeEnabled]
          -- XXX: if using > 0 we could run into a new tx not having utxo available situation?
          <> [(10, genNewTx) | length confirmedUTxO > 1]
          <> settlementActions headIdVar confirmedUTxO

    -- With 'concurrentSettlements', settlements are submitted and observed as
    -- separate actions so that several can be in flight when a fork hits; see
    -- 'SubmitDeposit'. Observation is weighted higher so most pending
    -- settlements do get observed within a sequence. Without, every
    -- settlement completes before the next action.
    --
    -- Decommits are weighted above deposits, and their observation above
    -- both. Only one decommit may be in flight ('DecommitAlreadyInFlight'),
    -- so each has to be observed before the next can be asked for, and at
    -- equal weights a run of average length fits barely one round trip. Any
    -- deposit can be asked for at any time, so it needs no such help. See
    -- 'Hydra.ModelSpec.reportWalkActionMix' for what this comes out as.
    settlementActions headIdVar confirmedUTxO
      | concurrentSettlements =
          -- Offered only when the precondition can hold. Offering it while a
          -- decommit is in flight spends the draw on an action that is then
          -- discarded, which is where most of the shortfall used to go.
          [(5, genSubmitDecommit) | length confirmedUTxO > 1, null pendingDecommits]
            <> [(8, genObserveDecommitFinalized) | not $ null pendingDecommits]
            <> [(2, genSubmitDeposit headIdVar) | not $ null availableToDeposit]
            <> [(3, genObserveCommitFinalized) | not $ null pendingCommits]
      | otherwise =
          [(4, genDecommit) | length confirmedUTxO > 1]
            <> [(2, genDeposit headIdVar) | not $ null availableToDeposit]

    -- NOTE: Deposits all of one signer's available UTxO at once, see
    -- 'availableToDeposit'. Only signers with available UTxO qualify: an
    -- empty deposit is rejected at draft time (SnapshotIncrementUTxOIsNull).
    genDeposit headIdVar = do
      sk <- elements (nub $ fst <$> availableToDeposit)
      let utxoToDeposit = filter ((sk ==) . fst) availableToDeposit
      pure $ Some Deposit{headIdVar, utxoToDeposit}

    genDecommit =
      genPayment st >>= \(party, tx) -> pure . Some $ Decommit party tx

    genSubmitDeposit headIdVar = do
      sk <- elements (nub $ fst <$> availableToDeposit)
      let utxoToDeposit = filter ((sk ==) . fst) availableToDeposit
      pure $ Some $ SubmitDeposit headIdVar utxoToDeposit

    genObserveCommitFinalized =
      Some . ObserveCommitFinalized . fst <$> elements pendingCommits

    genSubmitDecommit =
      genPayment st >>= \(party, tx) -> pure . Some $ SubmitDecommit party tx

    genObserveDecommitFinalized =
      Some . ObserveDecommitFinalized . fst <$> elements pendingDecommits

    genNewTx = genPayment st >>= \(party, transaction) -> pure . Some $ NewTx party transaction

    genClose =
      Some . Close . deriveParty . fst <$> elements hydraParties

    genFanout =
      Some . Fanout . deriveParty . fst <$> elements hydraParties

    genRollbackAndForward = do
      numberOfBlocks <- choose (1, 2)
      pure . Some $ RollbackAndForward (wordToNatural numberOfBlocks)

    genRollbackAndFork = do
      -- Deep enough to reach a settlement observed a couple of blocks ago
      -- while a later one is still in flight.
      numberOfBlocks <- choose (1, 4)
      -- Only the concurrent walk relies on the nodes re-posting erased
      -- settlements; the sequential one lets the mempool re-land them.
      requeueErased <-
        if concurrentSettlements
          then elements [RequeueAll, RequeueDeposits, RequeueNone]
          else pure RequeueAll
      pure . Some $ RollbackAndFork{numberOfBlocks = wordToNatural numberOfBlocks, requeueErased}

    genRestartNode =
      Some . RestartNode . deriveParty . fst <$> elements hydraParties

  precondition WorldState{hydraState = Start} Seed{} =
    True
  precondition WorldState{hydraState = Idle{idleParties}} (Init p) =
    p `elem` idleParties
  precondition WorldState{hydraState = Open{headParameters}} Close{party} =
    party `elem` headParameters.parties
  precondition WorldState{hydraState = Open{headParameters, offChainState}} (NewTx party tx) =
    party `elem` headParameters.parties
      && (from tx, value tx) `List.elem` confirmedUTxO offChainState
  precondition _ Wait{} =
    True
  precondition WorldState{hydraState = Open{headIdVar}} Deposit{headIdVar = var, utxoToDeposit} =
    var == headIdVar
      -- An empty deposit is rejected at draft time; also keeps shrinking from
      -- emptying a deposit's utxo.
      && not (null utxoToDeposit)
  precondition WorldState{hydraState = Open{headParameters, offChainState}} Decommit{party, decommitTx} =
    party `elem` headParameters.parties
      && (from decommitTx, value decommitTx) `List.elem` confirmedUTxO offChainState
  precondition WorldState{hydraState = Open{headIdVar}} (SubmitDeposit var utxoToDeposit) =
    var == headIdVar
      && not (null utxoToDeposit)
  precondition WorldState{hydraState = Open{}, pendingCommits} (ObserveCommitApproved var) =
    var `elem` (fst <$> pendingCommits)
  precondition WorldState{hydraState = Open{}, pendingCommits, settledCommits} (ObserveCommitFinalized var) =
    var `elem` (fst <$> pendingCommits) || var `elem` settledCommits
  precondition WorldState{hydraState = Open{headParameters, offChainState}, pendingDecommits} (SubmitDecommit party decommitTx) =
    party `elem` headParameters.parties
      && (from decommitTx, value decommitTx) `List.elem` confirmedUTxO offChainState
      -- A decommit requested while another one is unsettled is rejected right
      -- away ('DecommitAlreadyInFlight'): decommits are sequential by design.
      -- One requested while a deposit is unsettled is fine, see
      -- 'performSubmitDecommit'.
      && null pendingDecommits
  precondition WorldState{hydraState = Open{}, pendingDecommits, settledDecommits} (ObserveDecommitFinalized var) =
    var `elem` (fst <$> pendingDecommits) || var `elem` settledDecommits
  precondition WorldState{hydraState = Open{}} (ObserveConfirmedTx _) =
    True
  precondition WorldState{hydraState = Open{}} ObserveHeadIsOpen =
    True
  precondition WorldState{hydraState = Closed{headParameters, fanoutDriving}} (Fanout party) =
    party `elem` headParameters.parties
      && fanoutDriving == FanoutNotStarted
  precondition WorldState{hydraState = Closed{headParameters, fanoutDriving}} (StartFanout party) =
    party `elem` headParameters.parties
      && fanoutDriving == FanoutNotStarted
  precondition WorldState{hydraState = Closed{headParameters, fanoutDriving, closedUTxO, fannedOut}} (PartialFanoutStep party selection) =
    party `elem` headParameters.parties
      && fanoutDriving /= FanoutAutoDraining
      && not (null selection)
      && all (`elem` (closedUTxO \\ fannedOut)) selection
  precondition WorldState{hydraState = Closed{fanoutDriving}} (ObservePartialFanoutSteps n) =
    fanoutDriving /= FanoutNotStarted && n > 0
  precondition WorldState{hydraState = Closed{headParameters, fanoutDriving}} (ObserveFanoutFinalized party) =
    party `elem` headParameters.parties
      && fanoutDriving /= FanoutNotStarted
  precondition WorldState{hydraState = Open{headParameters, onChainVersion}, pendingCommits, pendingDecommits} (CloseWithInitialSnapshot p) =
    -- Only head members have a node to close with; keeps shrinking from
    -- rebinding the action to a party outside the (shrunk) seed. Closing with
    -- the initial snapshot (and open version 0) is only valid on-chain while
    -- no increment or decrement has settled — nor is about to.
    p `elem` headParameters.parties
      && onChainVersion == 0
      && null pendingCommits
      && null pendingDecommits
  -- A fork while the fanout is in progress: the node has to re-post the step
  -- that was erased.
  precondition WorldState{hydraState = Closed{fanoutDriving}} RollbackAndFork{} =
    fanoutDriving /= FanoutNotStarted
  precondition WorldState{hydraState = Open{}, pendingCommits} RollbackAndFork{requeueErased} =
    -- A fork that drops deposit transactions for good may hit one that is
    -- only a few blocks old: those funds are then simply gone from L1 (the
    -- depositor would have to deposit again), which the model does not track.
    -- Settled deposits are safe: their deposit transaction precedes the
    -- increment by at least the activation period (5 blocks), more than the
    -- generated fork depth.
    requeueErased /= RequeueNone || null pendingCommits
  precondition WorldState{hydraState = Open{headParameters}} (RestartNode p) =
    p `elem` headParameters.parties
  precondition WorldState{hydraState} (RollbackAndForward _) =
    case hydraState of
      Start{} -> False
      Idle{} -> False
      Open{} -> True
      Closed{} -> True
      Final{} -> False
  precondition _ StopTheWorld =
    True
  precondition _ _ =
    False

  nextState s@WorldState{hydraState, availableToDeposit, pendingCommits, pendingDecommits, settledCommits, settledDecommits} a result =
    case a of
      Seed{seedKeys, contestationPeriod, additionalUTxO, concurrentSettlements, faultyParty = _} ->
        s{hydraParties = seedKeys, hydraState = idleState, availableToDeposit = additionalUTxO, concurrentSettlements}
       where
        idleState = Idle{idleParties, cardanoKeys, contestationPeriod}
        idleParties = map (deriveParty . fst) seedKeys
        cardanoKeys = map (\(_, CardanoSigningKey sk) -> getVerificationKey sk) seedKeys
      Init{} ->
        s{hydraState = mkInitialState hydraState}
       where
        mkInitialState = \case
          Idle{idleParties, contestationPeriod} ->
            Open
              { headIdVar = result
              , headParameters =
                  HeadParameters
                    { parties = idleParties
                    , contestationPeriod = contestationPeriod
                    , depositPeriod = defaultDepositPeriod
                    }
              , offChainState = OffChainState{confirmedUTxO = mempty}
              , committed = mempty
              , onChainVersion = 0
              }
          _ -> error "unexpected state"
      Deposit{utxoToDeposit} ->
        s
          { hydraState = settleCommit utxoToDeposit hydraState
          , availableToDeposit = availableToDeposit \\ utxoToDeposit
          }
      Decommit _party tx ->
        s{hydraState = settleDecommit tx (removeDecommitted tx hydraState)}
      -- The deposit leaves the pool right away, but only counts as in the
      -- head (and bumps the on-chain version) once observed as finalized.
      SubmitDeposit _ utxoToDeposit ->
        s
          { availableToDeposit = availableToDeposit \\ utxoToDeposit
          , pendingCommits = (result, utxoToDeposit) : pendingCommits
          }
      ObserveCommitApproved _ -> s
      ObserveCommitFinalized var ->
        case List.lookup var pendingCommits of
          -- Re-observation of an already settled commit: only count it.
          Nothing -> s{settledCommits = var : settledCommits}
          Just utxo ->
            s
              { hydraState = settleCommit utxo hydraState
              , pendingCommits = filter ((/= var) . fst) pendingCommits
              , settledCommits = var : settledCommits
              }
      -- The decommitted output leaves the L2 ledger with the snapshot (which
      -- 'SubmitDecommit' waits for), the on-chain version bumps with the
      -- decrement.
      SubmitDecommit _ tx ->
        s
          { hydraState = removeDecommitted tx hydraState
          , pendingDecommits = (result, tx) : pendingDecommits
          }
      ObserveDecommitFinalized var ->
        case List.lookup var pendingDecommits of
          Nothing -> s{settledDecommits = var : settledDecommits}
          Just tx ->
            s
              { hydraState = settleDecommit tx hydraState
              , pendingDecommits = filter ((/= var) . fst) pendingDecommits
              , settledDecommits = var : settledDecommits
              }
      Close{} ->
        closeWith hydraState
      Fanout{} ->
        s{hydraState = updateWithFanout hydraState}
      ObserveFanoutFinalized{} ->
        s{hydraState = updateWithFanout hydraState}
      StartFanout{} ->
        s{hydraState = startAutoFanout hydraState}
      PartialFanoutStep _ selection ->
        s{hydraState = manualFanoutStep selection hydraState}
      ObservePartialFanoutSteps{} -> s
      (NewTx _ tx) ->
        s{hydraState = updateWithNewTx hydraState}
       where
        updateWithNewTx = \case
          hs@Open{offChainState = OffChainState{confirmedUTxO}} ->
            hs
              { offChainState =
                  OffChainState
                    { confirmedUTxO = confirmedUTxO `applyTx` tx
                    }
              }
          _ -> error "unexpected state"
      CloseWithInitialSnapshot _ ->
        closeWith hydraState
      RollbackAndForward _numberOfBlocks -> s
      RollbackAndFork{} -> s
      RestartNode{} -> s
      Wait _ -> s
      ObserveConfirmedTx _ -> s
      ObserveHeadIsOpen -> s
      StopTheWorld -> s
   where
    updateWithFanout = \case
      Closed{closedUTxO, unsettledAtClose} -> Final{finalUTxO = closedUTxO, unsettledAtFinal = unsettledAtClose}
      _ -> error "unexpected state"

    startAutoFanout = \case
      c@Closed{} -> c{fanoutDriving = FanoutAutoDraining}
      _ -> error "unexpected state"

    manualFanoutStep selection = \case
      c@Closed{fannedOut} -> c{fanoutDriving = FanoutManual, fannedOut = fannedOut <> selection}
      _ -> error "unexpected state"

    -- Closing settles the pending lists: whatever was still in flight may or
    -- may not make it into the head, see 'unsettledAtClose'.
    closeWith = \case
      Open{offChainState = OffChainState{confirmedUTxO}, headParameters} ->
        s
          { hydraState =
              Closed
                { headParameters
                , closedUTxO = confirmedUTxO
                , unsettledAtClose =
                    concatMap snd pendingCommits
                      <> [(to, value) | (_, Payment{to, value}) <- pendingDecommits]
                , fanoutDriving = FanoutNotStarted
                , fannedOut = mempty
                }
          , pendingCommits = mempty
          , pendingDecommits = mempty
          }
      _ -> error "unexpected state"

  shrinkAction _ctx _st = \case
    seed@Seed{seedKeys, additionalUTxO} -> do
      seedKeys' <- shrink seedKeys
      guard $ length seedKeys' < length seedKeys
      let cardanoKeys' = snd <$> seedKeys'
      pure $ Some $ seed{seedKeys = seedKeys', additionalUTxO = filter ((`elem` cardanoKeys') . fst) additionalUTxO}
    _other -> []

-- | Add a finalized commit to the head's UTxO and bump the on-chain version.
settleCommit :: UTxOType Payment -> GlobalState -> GlobalState
settleCommit utxo = \case
  hs@Open{offChainState = OffChainState{confirmedUTxO}, onChainVersion} ->
    hs
      { offChainState = OffChainState{confirmedUTxO = utxo <> confirmedUTxO}
      , onChainVersion = onChainVersion + 1
      }
  _ -> error "unexpected state"

-- | Remove a decommitted output from the head's UTxO (it leaves the L2 ledger
-- with the snapshot carrying the decommit).
removeDecommitted :: Payment -> GlobalState -> GlobalState
removeDecommitted tx = \case
  hs@Open{offChainState = OffChainState{confirmedUTxO}} ->
    hs{offChainState = OffChainState{confirmedUTxO = List.delete (from tx, value tx) confirmedUTxO}}
  _ -> error "unexpected state"

-- | Account for a finalized decrement: bump the on-chain version.
settleDecommit :: Payment -> GlobalState -> GlobalState
settleDecommit _ = \case
  hs@Open{onChainVersion} -> hs{onChainVersion = onChainVersion + 1}
  _ -> error "unexpected state"

instance HasVariables WorldState where
  getAllVariables WorldState{hydraState, pendingCommits, pendingDecommits, settledCommits, settledDecommits} =
    Set.fromList (Some . fst <$> pendingCommits)
      <> Set.fromList (Some . fst <$> pendingDecommits)
      <> Set.fromList (Some <$> settledCommits)
      <> Set.fromList (Some <$> settledDecommits)
      <> case hydraState of
        Open{headIdVar} -> Set.singleton $ Some headIdVar
        _ -> mempty

instance HasVariables (Action WorldState a) where
  getAllVariables = \case
    Deposit{headIdVar} -> Set.singleton $ Some headIdVar
    SubmitDeposit headIdVar _ -> Set.singleton $ Some headIdVar
    ObserveCommitApproved var -> Set.singleton $ Some var
    ObserveCommitFinalized var -> Set.singleton $ Some var
    ObserveDecommitFinalized var -> Set.singleton $ Some var
    ObserveConfirmedTx tx -> Set.singleton $ Some tx
    _other -> mempty

deriving stock instance Show (Action WorldState a)
deriving stock instance Eq (Action WorldState a)

-- ** Generator Helper

-- | Whether random 'RestartNode' actions are generated. On: a restarted node
-- recovers head state (event store), chain point, network consumer offset
-- (etcd-style) and the full chain-sync 'localChainState' history, so it
-- converges under load like a real fail-recovery. Flip to 'False' to drop the
-- fail-recovery dimension if it ever proves flaky.
restartNodeEnabled :: Bool
restartNodeEnabled = True

-- | The default seed settles each deposit and decommit before the next
-- action, see 'concurrentSettlements'.
genSeed :: Gen (Action WorldState ())
genSeed = genSeedWith False

-- | 'genSeedWith' with one of the parties misbehaving on the network in the
-- given way, see 'FaultMode'.
genFaultySeed :: Bool -> FaultMode -> Gen (Action WorldState ())
genFaultySeed concurrentSettlements mode = do
  seed <- genSeedWith concurrentSettlements
  case seed of
    Seed{seedKeys} -> do
      (hsk, _) <- elements seedKeys
      pure seed{faultyParty = Just (deriveParty hsk, mode)}
    _ -> pure seed

genSeedWith :: Bool -> Gen (Action WorldState ())
genSeedWith concurrentSettlements = do
  seedKeys <- resize maximumNumberOfParties partyKeys
  contestationPeriod <- genContestationPeriod
  -- NOTE: Unique (signer, value) pairs: 'toRealUTxO' derives mocked TxIns
  -- from them, so duplicates deposited in separate actions would collide.
  additionalUTxO <- fmap nub . listOf $ do
    sk <- snd <$> elements seedKeys
    value <- genAdaValue
    pure (sk, value)
  pure $ Seed{seedKeys, contestationPeriod, additionalUTxO, concurrentSettlements, faultyParty = Nothing}

genContestationPeriod :: Gen ContestationPeriod
genContestationPeriod =
  chooseEnum (1, 200)

genInit :: [(Secret (SigningKey HydraKey), b)] -> Gen (Action WorldState HeadId)
genInit hydraParties = do
  key <- fst <$> elements hydraParties
  let party = deriveParty key
  pure $ Init party

genPayment :: WorldState -> Gen (Party, Payment)
genPayment WorldState{hydraParties, hydraState} =
  case hydraState of
    Open{offChainState = OffChainState{confirmedUTxO}} -> do
      let spendable =
            mapMaybe
              ( \(from, value) ->
                  (from,value,) . deriveParty . fst <$> List.find ((== from) . snd) hydraParties
              )
              $ filter (not . null . toList . snd) confirmedUTxO
      case spendable of
        [] -> discard
        _ -> do
          (from, value, party) <- elements spendable
          -- NOTE: It's perfectly possible this yields a payment to self and it
          -- assumes hydraParties is not empty else `elements` will crash
          (_, to) <- elements hydraParties
          pure (party, Payment{from, to, value})
    _ -> error $ "genPayment impossible in state: " <> show hydraState

unsafeConstructorName :: Show a => a -> String
unsafeConstructorName = Prelude.head . Prelude.words . show

-- | Generate a list of pairs of Hydra/Cardano signing keys.
--  All the keys in this list are guaranteed to be unique.
partyKeys :: Gen [(Secret (SigningKey HydraKey), CardanoSigningKey)]
partyKeys =
  sized $ \len -> do
    numParties <- choose (1, len)
    hks <- nub <$> vectorOf numParties arbitrary
    cks <- nub . fmap (CardanoSigningKey . mkSecret) <$> vectorOf numParties genSigningKey
    pure $ zip hks cks

-- | Exactly @n@ distinct parties, for scripted scenarios (see 'partyKeys').
genPartyKeysExactly :: Int -> Gen [(Secret (SigningKey HydraKey), CardanoSigningKey)]
genPartyKeysExactly n =
  gen `suchThat` ((== n) . length)
 where
  gen = do
    hks <- nub <$> vectorOf n arbitrary
    cks <- nub . fmap (CardanoSigningKey . mkSecret) <$> vectorOf n genSigningKey
    pure $ zip hks cks

-- * Running the model

-- | Concrete state needed to run actions against the implementation.
-- This state is used and might be updated when actually `perform`ing actions generated from the `StateModel`.
data Nodes m = Nodes
  { nodes :: Map.Map Party (TestHydraClient Tx m)
  -- ^ Map from party identifiers to a /handle/ for interacting with a node.
  , logger :: Tracer m (HydraLog Tx)
  -- ^ Logger used by each node.
  -- The reason we put this here is because the concrete value needs to be
  -- instantiated upon the test run initialisation, outiside of the model.
  , threads :: [Async m ()]
  -- ^ List of threads spawned when executing `RunMonad`
  , chain :: SimulatedChainNetwork Tx m
  , eventStores :: Map.Map Party (EventStore (StateEvent Tx) m, m [StateEvent Tx])
  -- ^ Each node's event store (with a direct reader), so 'RestartNode' can
  -- recover a node from its own persisted events like fail-recovery would.
  , nodeThreads :: Map.Map Party (Async m ())
  -- ^ Each node's main thread, so 'RestartNode' can crash one selectively.
  }

-- NOTE: This newtype is needed to allow its use in typeclass instances
newtype RunState m = RunState {nodesState :: TVar m (Nodes m)}

-- | Our execution `MonadTrans`former.
--
-- This type is needed in order to keep the execution monad `m` abstract  and thus
-- simplify the definition of the `RunModel` instance which requires a proper definition
-- of `Realized`  type family. See [this issue](https://github.com/input-output-hk/quickcheck-dynamic/issues/29)
-- for a discussion on why this monad is needed.
--
-- We could perhaps getaway with it and just have a type based on `IOSim` monad
-- but this is cumbersome to write.
newtype RunMonad m a = RunMonad {runMonad :: ReaderT (RunState m) m a}
  deriving newtype (Functor, Applicative, Monad, MonadReader (RunState m), MonadThrow, MonadCatch, MonadTime)

instance MonadTrans RunMonad where
  lift = RunMonad . lift

instance MonadSTM m => MonadState (Nodes m) (RunMonad m) where
  get = ask >>= lift . readTVarIO . nodesState

  put n = ask >>= lift . atomically . flip modifyTVar (const n) . nodesState

data RunException
  = TransactionNotObserved Payment UTxO
  | UnexpectedParty Party
  | UnknownAddress AddressInEra [(AddressInEra, CardanoSigningKey)]
  | CannotFindSpendableUTxO Payment UTxO
  deriving stock (Eq, Show)

instance Exception RunException

-- | This type family is needed to link the _actual_ output from running actions
-- with the ones that are modelled.
--
-- In our case we can keep things simple and use the same types on both side of
-- the fence.
type instance Realized (RunMonad m) a = a

-- NOTE: Sort `[TxOut]` by the address and values. We want to make
-- sure that the fanout outputs match what we had in the open Head
-- exactly.
sortTxOuts :: [TxOut ctx] -> [TxOut ctx]
sortTxOuts = sortOn (\o -> (txOutAddress o, selectLovelace (txOutValue o)))

instance
  ( MonadAsync m
  , MonadFork m
  , MonadMask m
  , MonadTimer m
  , MonadThrow (STM m)
  , MonadLabelledSTM m
  , MonadDelay m
  , MonadTime m
  ) =>
  RunModel WorldState (RunMonad m)
  where
  postcondition (_, st) action _lookup result = do
    counterexamplePost "Postcondition failed"
    counterexamplePost ("Action:   " <> show action)
    counterexamplePost ("State:    " <> show st)

    case action of
      Fanout{} -> fanoutDistributedEverything result
      ObserveFanoutFinalized{} -> fanoutDistributedEverything result
      _ -> pure True
   where
    -- The fanout must distribute everything confirmed in the head, and nothing
    -- else but outputs of settlements that were still pending at close (see
    -- 'unsettledAtClose').
    fanoutDistributedEverything :: UTxO -> PostconditionM (RunMonad m) Bool
    fanoutDistributedEverything distributed =
      case hydraState st of
        Final{finalUTxO, unsettledAtFinal} -> do
          let expected = sortTxOuts (toTxOuts finalUTxO)
              actual = sortTxOuts (snd <$> UTxO.toList distributed)
              missing = expected \\ actual
              unexpected = (actual \\ expected) \\ sortTxOuts (toTxOuts unsettledAtFinal)
          counterexamplePost ("Missing from fanout:    " <> show missing)
          counterexamplePost ("Unexpected in fanout:   " <> show unexpected)
          pure (null missing && null unexpected)
        _ -> pure False

  monitoring (s, s') _action _lookup _result =
    decorateTransitions
   where
    decorateTransitions =
      case (hydraState s, hydraState s') of
        (st, st') -> tabulate "Transitions" [unsafeConstructorName st <> " -> " <> unsafeConstructorName st']

  perform st action lookup = do
    case action of
      Seed{seedKeys, contestationPeriod, faultyParty} ->
        seedWorld seedKeys contestationPeriod faultyParty
      Init party ->
        performInit party
      Deposit headIdVar utxo -> do
        let headId = lookup headIdVar
        performDeposit headId utxo
      Decommit party tx ->
        performDecommit party tx
      SubmitDeposit headIdVar utxo ->
        performSubmitDeposit (lookup headIdVar) utxo
      ObserveCommitApproved var ->
        performObserveCommitApproved (fromMaybe mempty $ List.lookup var (pendingCommits st))
      ObserveCommitFinalized var ->
        -- The n-th observation of this commit waits for its n-th report.
        performObserveCommitFinalized (1 + length (filter (== var) (settledCommits st))) (lookup var)
      SubmitDecommit party tx ->
        performSubmitDecommit party tx
      ObserveDecommitFinalized var ->
        performObserveDecommitFinalized (1 + length (filter (== var) (settledDecommits st))) (lookup var)
      Close party ->
        performClose party
      Fanout party ->
        performFanout party
      StartFanout party ->
        performStartFanout party
      PartialFanoutStep party selection ->
        performPartialFanoutStep party selection
      ObservePartialFanoutSteps n ->
        performObservePartialFanoutSteps n
      ObserveFanoutFinalized party ->
        performObserveFanoutFinalized party
      NewTx party transaction ->
        performNewTx party transaction
      Wait delay ->
        lift $ threadDelay delay
      ObserveConfirmedTx var -> do
        let tx = lookup var
        nodes <- Map.toList <$> gets nodes
        forM_ nodes $ \(_, node) -> do
          lift (waitForUTxOToSpend mempty (to tx) (value tx) node) >>= \case
            Left u -> throwIO $ TransactionNotObserved tx u
            Right _ -> pure ()
      ObserveHeadIsOpen -> do
        nodes' <- Map.toList <$> gets nodes
        forM_ nodes' $ \(_, node) -> do
          outputs <- lift $ serverOutputs node
          case find headIsOpen outputs of
            Just _ -> pure ()
            Nothing -> error "The head is not open for node"
      CloseWithInitialSnapshot party ->
        performCloseWithInitialSnapshot st party
      RollbackAndForward numberOfBlocks ->
        performRollbackAndForward numberOfBlocks
      RollbackAndFork{numberOfBlocks, requeueErased} ->
        performRollbackAndFork numberOfBlocks requeueErased
      RestartNode party ->
        performRestartNode st party
      StopTheWorld ->
        stopTheWorld

-- ** Performing actions

-- | Deposit period used by all nodes and the 'performDeposit'.
testDepositPeriod :: DepositPeriod
testDepositPeriod = 100

seedWorld ::
  ( MonadAsync m
  , MonadTimer m
  , MonadThrow (STM m)
  , MonadLabelledSTM m
  , MonadFork m
  , MonadMask m
  , MonadDelay m
  , MonadTime m
  ) =>
  [(Secret (SigningKey HydraKey), CardanoSigningKey)] ->
  ContestationPeriod ->
  Maybe (Party, FaultMode) ->
  RunMonad m ()
seedWorld seedKeys seedCP faulty = do
  tr <- gets logger

  mockChain@SimulatedChainNetwork{tickThread} <-
    lift $ mockChainAndNetwork (contramap DirectChain tr) seedKeys faulty
  pushThread tickThread

  perNode <- forM seedKeys $ \(hsk, _csk) -> do
    let party = deriveParty hsk
        otherParties = filter (/= party) parties
    eventStore <- lift createMockEventStoreWithReader
    (testClient, nodeThread) <- startNode tr mockChain seedCP eventStore hsk otherParties
    pushThread nodeThread
    pure (party, (testClient, eventStore, nodeThread))

  modify $ \n ->
    n
      { nodes = Map.fromList [(party, c) | (party, (c, _, _)) <- perNode]
      , eventStores = Map.fromList [(party, es) | (party, (_, es, _)) <- perNode]
      , nodeThreads = Map.fromList [(party, t) | (party, (_, _, t)) <- perNode]
      , chain = mockChain
      }
 where
  parties = map (deriveParty . fst) seedKeys

  pushThread :: MonadSTM m => Async m () -> RunMonad m ()
  pushThread t = modify $ \s ->
    s{threads = t : threads s}

-- | (Re-)create and start a single hydra node on the given event store,
-- recovering its state from the store's events, and wait for it to be in sync
-- with the chain. Shared by 'seedWorld' and 'performRestartNode'.
startNode ::
  ( MonadAsync m
  , MonadLabelledSTM m
  , MonadFork m
  , MonadDelay m
  , MonadMask m
  , MonadTime m
  ) =>
  Tracer m (HydraLog Tx) ->
  SimulatedChainNetwork Tx m ->
  ContestationPeriod ->
  (EventStore (StateEvent Tx) m, m [StateEvent Tx]) ->
  Secret (SigningKey HydraKey) ->
  [Party] ->
  RunMonad m (TestHydraClient Tx m, Async m ())
startNode tr mockChain seedCP (eventStore, readEvents) hsk otherParties = lift $ do
  outputs <- newLabelledTQueueIO ("seed-world-outputs-" <> shortLabel hsk)
  messages <- newLabelledTQueueIO ("seed-world-messages-" <> shortLabel hsk)
  outputHistory <- newLabelledTVarIO "seed-world-output-history" []
  events <- readEvents
  node@HydraNode{nodeStateHandler = NodeStateHandler{queryNodeState}} <-
    createHydraNodeWithEventStore
      eventStore
      events
      (contramap Node tr)
      ledger
      initialChainState
      hsk
      otherParties
      outputs
      messages
      outputHistory
      mockChain
      seedCP
      testDepositPeriod
  nodeThread <- asyncLabelled ("seed-world-node-" <> shortLabel hsk) $ runHydraNode node
  link nodeThread
  -- await for the node to be in sync with the chain before returning the client
  atomically $ do
    st <- queryNodeState
    case st of
      NodeInSync{} -> pure ()
      _ -> retry
  let testClient = createTestHydraClient outputs messages outputHistory node
  pure (testClient, nodeThread)
 where
  ledger = cardanoLedger defaultGlobals defaultLedgerEnv

performDeposit ::
  (MonadThrow m, MonadTimer m, MonadAsync m, MonadTime m, MonadLabelledSTM m) =>
  HeadId ->
  [(CardanoSigningKey, Value)] ->
  RunMonad m ()
performDeposit headId utxoToDeposit = do
  nodes <- gets nodes
  SimulatedChainNetwork{simulateDeposit} <- gets chain
  deadline <- depositDeadline
  lift $ do
    txid <- simulateDeposit headId (toRealUTxO utxoToDeposit) deadline
    waitUntilMatch (elems nodes) $ \case
      -- NOTE: We are fine with only recorded outputs if the utxo is not
      -- actually adding something. Honest nodes would not try to
      -- snapshot/increment this.
      CommitRecorded{} | null utxoToDeposit -> Just ()
      CommitFinalized{depositTxId} -> guard $ txid == depositTxId
      _ -> Nothing

-- | Deadline for deposits made by the model: far enough in the future that the
-- deposit is still claimable once it activates (at @created +
-- depositActivation@, with @created@ up to half a deposit period ahead of
-- submission) even when several deposits settle one after the other, each
-- taking a few blocks. It expires at @deadline - depositPeriod@.
depositDeadline :: MonadTime m => RunMonad m UTCTime
depositDeadline = addUTCTime (8 * toNominalDiffTime testDepositPeriod) <$> getCurrentTime

-- | Submit a deposit and wait until every node has recorded it on chain. Its
-- settlement is observed separately, see 'performObserveCommitFinalized'.
performSubmitDeposit ::
  (MonadThrow m, MonadTimer m, MonadDelay m, MonadTime m) =>
  HeadId ->
  [(CardanoSigningKey, Value)] ->
  RunMonad m TxId
performSubmitDeposit headId utxoToDeposit = do
  nodes <- gets nodes
  SimulatedChainNetwork{simulateDeposit} <- gets chain
  deadline <- depositDeadline
  lift $ do
    txid <- simulateDeposit headId (toRealUTxO utxoToDeposit) deadline
    waitForOutputs ("deposit " <> show txid <> " recorded") 1 (elems nodes) $ \case
      CommitRecorded{pendingDeposit} -> txid == pendingDeposit
      _ -> False
    pure txid

-- | Wait until every node has confirmed a snapshot claiming the given deposit
-- ('CommitApproved'): the increment is now in flight.
performObserveCommitApproved ::
  (MonadThrow m, MonadTimer m, MonadDelay m) =>
  UTxOType Payment ->
  RunMonad m ()
performObserveCommitApproved deposited = do
  nodes <- gets nodes
  let expected = sortTxOuts (UTxO.txOutputs (toRealUTxO deposited))
  lift . waitForOutputs "commit approved" 1 (elems nodes) $ \case
    CommitApproved{utxoToCommit} -> sortTxOuts (UTxO.txOutputs utxoToCommit) == expected
    _ -> False

-- | Wait until every node has reported the increment claiming the given
-- deposit for the n-th time. A wedged settlement surfaces here as a timeout.
performObserveCommitFinalized ::
  (MonadThrow m, MonadTimer m, MonadDelay m) =>
  Int ->
  TxId ->
  RunMonad m ()
performObserveCommitFinalized n txid = do
  nodes <- gets nodes
  lift . waitForOutputs ("commit " <> show txid <> " finalized (" <> show n <> ". time)") n (elems nodes) $ \case
    CommitFinalized{depositTxId} -> txid == depositTxId
    _ -> False

-- | Wait until every node's output history holds at least @n@ outputs
-- matching the predicate, or fail after 'observationTimeout'.
--
-- Unlike 'waitUntilMatch' this does not consume outputs, so settlements can
-- be observed in any order (several may be in flight and finalize in an order
-- the model does not control) and repeatedly (a settlement re-landing after a
-- fork is reported again). It also does not swallow reports that arrive
-- earlier than expected, e.g. a decrement observed on chain before its
-- snapshot confirmed locally.
waitForOutputs ::
  (MonadThrow m, MonadTimer m, MonadDelay m) =>
  String ->
  Int ->
  [TestHydraClient Tx m] ->
  (ServerOutput Tx -> Bool) ->
  m ()
waitForOutputs what n nodes p =
  waitUntilHistory (what <> " " <> show n <> " time(s)") nodes $ \outs ->
    length (filter p outs) >= n

-- | Wait until every node's output history satisfies the predicate, or fail
-- after 'observationTimeout'. See 'waitForOutputs'.
waitUntilHistory ::
  (MonadThrow m, MonadTimer m, MonadDelay m) =>
  String ->
  [TestHydraClient Tx m] ->
  ([ServerOutput Tx] -> Bool) ->
  m ()
waitUntilHistory what nodes p =
  timeout observationTimeout (forM_ nodes waitOne) >>= \case
    Just () -> pure ()
    Nothing -> do
      satisfied <- forM nodes (fmap p . serverOutputs)
      failure $
        "waitUntilHistory: not all nodes reported " <> what <> " within " <> show observationTimeout <> "; per node: " <> show satisfied
 where
  waitOne node = do
    outs <- serverOutputs node
    unless (p outs) $ threadDelay 1 >> waitOne node

-- | How long an observation waits for the nodes to report something. Every
-- observed step (a settlement landing, re-landing after a fork, a snapshot
-- confirming) takes a handful of blocks of 20s, so an hour is generous while
-- still failing a wedged head reasonably fast.
observationTimeout :: DiffTime
observationTimeout = 3600

-- | Request a decommit and wait until every node has confirmed the snapshot
-- carrying it ('DecommitApproved'), i.e. the outputs have left the L2 ledger
-- and the decrement is in flight. Its settlement is observed separately, see
-- 'performObserveDecommitFinalized'.
performSubmitDecommit ::
  forall m.
  (MonadThrow m, MonadTimer m, MonadDelay m) =>
  Party ->
  Payment ->
  RunMonad m UTxO
performSubmitDecommit party tx = do
  nodes <- gets nodes
  let thisNode = nodes ! party
  waitForOpen thisNode

  (i, o) <-
    lift (waitForUTxOToSpend mempty (from tx) (value tx) thisNode) >>= \case
      Left u -> error $ "Cannot execute SubmitDecommit for " <> show tx <> ", no spendable UTxO in " <> show u
      Right ok -> pure ok

  let realTx =
        either
          (error . show)
          id
          (case from tx of CardanoSigningKey sk -> mkSimpleTx (i, o) (decommitRecipient tx, value tx) sk)

  let decommitted = utxoFromTx realTx
      decommitTxId = getTxId (getTxBody realTx)
      -- NOTE: Sync on the confirmed snapshot carrying the decommit rather than
      -- on 'DecommitApproved', which not every node emits.
      approved = \case
        SnapshotConfirmed{snapshot} ->
          (sortTxOuts . UTxO.txOutputs <$> Snapshot.utxoToDecommit snapshot) == Just (sortTxOuts (UTxO.txOutputs decommitted))
        _ -> False
  -- A decommit requested while a deposit is unsettled is recorded on every
  -- node and proposed once the commit landed, so one request is enough.
  party `sendsInput` Input.Decommit realTx
  lift . waitUntilHistory ("snapshot with decommit " <> show decommitTxId <> " confirmed") (elems nodes) $ \outs ->
    any approved outs
  pure decommitted

-- | Wait until every node has reported the decrement distributing the given
-- decommitted UTxO for the n-th time.
performObserveDecommitFinalized ::
  (MonadThrow m, MonadTimer m, MonadDelay m) =>
  Int ->
  UTxO ->
  RunMonad m ()
performObserveDecommitFinalized n decommitted = do
  nodes <- gets nodes
  lift . waitForOutputs ("decommit finalized (" <> show n <> ". time)") n (elems nodes) $ \case
    DecommitFinalized{distributedUTxO} ->
      sortTxOuts (UTxO.txOutputs distributedUTxO) == sortTxOuts (UTxO.txOutputs decommitted)
    _ -> False

decommitRecipient :: Payment -> AddressInEra
decommitRecipient tx = case to tx of
  CardanoSigningKey sk -> mkVkAddress testNetworkId (getVerificationKey sk)

performDecommit ::
  (MonadThrow m, MonadTimer m, MonadAsync m, MonadDelay m, MonadLabelledSTM m) =>
  Party ->
  Payment ->
  RunMonad m ()
performDecommit party tx = do
  let recipient = case to tx of
        CardanoSigningKey sk -> mkVkAddress testNetworkId (getVerificationKey sk)
  nodes <- gets nodes
  let thisNode = nodes ! party
  waitForOpen thisNode

  (i, o) <-
    lift (waitForUTxOToSpend mempty (from tx) (value tx) thisNode) >>= \case
      Left u -> error $ "Cannot execute Decommit for " <> show tx <> ", no spendable UTxO in " <> show u
      Right ok -> pure ok

  let realTx =
        either
          (error . show)
          id
          (case from tx of CardanoSigningKey sk -> mkSimpleTx (i, o) (recipient, value tx) sk)

  party `sendsInput` Input.Decommit realTx

  lift . waitUntilMatch (elems nodes) $ \case
    DecommitFinalized{distributedUTxO} ->
      guard $ sortTxOuts (UTxO.txOutputs distributedUTxO) == sortTxOuts (UTxO.txOutputs $ utxoFromTx realTx)
    _ -> Nothing

performNewTx ::
  (MonadThrow m, MonadAsync m, MonadTimer m, MonadDelay m, MonadLabelledSTM m) =>
  Party ->
  Payment ->
  RunMonad m Payment
performNewTx party tx = do
  let recipient = case to tx of
        CardanoSigningKey sk -> mkVkAddress testNetworkId (getVerificationKey sk)
  nodes <- gets nodes
  let thisNode = nodes ! party
  waitForOpen thisNode

  (i, o) <-
    lift (waitForUTxOToSpend mempty (from tx) (value tx) thisNode) >>= \case
      Left u -> failure $ "Cannot execute NewTx for " <> show tx <> ", no spendable UTxO in " <> show u
      Right ok -> pure ok

  let realTx =
        either
          (error . show)
          id
          (case from tx of CardanoSigningKey sk -> mkSimpleTx (i, o) (recipient, value tx) sk)

  party `sendsInput` Input.NewTx realTx
  lift . waitUntilMatch (elems nodes) $ \case
    SnapshotConfirmed{snapshot = snapshot} ->
      guard $ realTx `elem` Snapshot.confirmed snapshot
    err@(TxInvalid{}) -> error ("expected tx to be valid: " <> show err)
    _ -> Nothing
  pure tx

-- | Wait for the head to be open by searching from the beginning. Note that
-- there rollbacks or multiple life-cycles of heads are not handled here.
waitForOpen :: MonadDelay m => TestHydraClient tx m -> RunMonad m ()
waitForOpen node = do
  outs <- lift $ serverOutputs node
  unless (any headIsOpen outs) waitAndRetry
 where
  waitAndRetry = lift (threadDelay 0.1) >> waitForOpen node

-- | Wait for the head to be closed by searching from the beginning. Note that
-- there rollbacks or multiple life-cycles of heads are not handled here.
waitForReadyToFanout :: MonadDelay m => TestHydraClient tx m -> RunMonad m ()
waitForReadyToFanout node = do
  outs <- lift $ serverOutputs node
  unless (any headIsReadyToFanout outs) waitAndRetry
 where
  waitAndRetry = lift (threadDelay 0.1) >> waitForReadyToFanout node

sendsInput :: forall m. (MonadSTM m, MonadThrow m, MonadDelay m) => Party -> ClientInput Tx -> RunMonad m ()
sendsInput party command = do
  actorNode <- getActorNode party
  -- A node rejects client inputs while catching up (e.g. right after a
  -- rollback, until the next block restores its view). A real client sees
  -- 'RejectedInputBecauseUnsynced' and retries; we wait for sync upfront.
  waitForInSync actorNode
  lift $ actorNode `send` command
 where
  waitForInSync :: TestHydraClient Tx m -> RunMonad m ()
  waitForInSync node =
    lift (queryState node) >>= \case
      NodeInSync{} -> pure ()
      _ -> lift (threadDelay 1) >> waitForInSync node

getActorNode :: (MonadSTM m, MonadThrow m) => Party -> RunMonad m (TestHydraClient Tx m)
getActorNode party = do
  nodes <- gets nodes
  case Map.lookup party nodes of
    Nothing -> throwIO $ UnexpectedParty party
    Just actorNode -> pure actorNode

performInit :: (MonadThrow m, MonadAsync m, MonadTimer m, MonadDelay m, MonadLabelledSTM m) => Party -> RunMonad m HeadId
performInit party = do
  party `sendsInput` Input.Init
  nodes <- gets nodes
  lift . waitUntilMatch (elems nodes) $ \case
    HeadIsOpen{headId} -> Just headId
    _ -> Nothing

performClose :: forall m. (MonadThrow m, MonadDelay m, MonadLabelledSTM m) => Party -> RunMonad m ()
performClose party = do
  nodes <- gets nodes
  let thisNode = nodes ! party
  waitForOpen thisNode
  -- A close posted while a settlement race is unresolved (e.g. the increment
  -- was observed on chain but its snapshot has not confirmed locally yet)
  -- fails on-chain and nothing in the node re-posts it: like a real client,
  -- retry until the head is closed. Success is detected by polling every
  -- node's head state for 'Closed' (not by matching a 'HeadIsClosed' server
  -- output, which 'waitUntilMatch' would consume — so a retry would then wait
  -- for a second one that never comes). Only (re-)send Close while this node's
  -- head is still open: once a close has landed, a slow (e.g. just-restarted)
  -- peer may still be catching up, and re-sending would yield a spurious
  -- CommandFailed on the already-closed head.
  let isClosed :: NodeState Tx -> Bool
      isClosed st' = case headState st' of
        HeadLogic.Closed{} -> True
        _ -> False
  let allClosed = lift $ all isClosed <$> mapM queryState (elems nodes)
  let closeWithRetry :: Int -> RunMonad m ()
      closeWithRetry n
        | n <= 0 = failure "performClose: head not closed after retries"
        | otherwise = do
            thisClosed <- lift $ not . isOpen <$> queryState thisNode
            unless thisClosed $ party `sendsInput` Input.Close
            -- Poll for all nodes closed, giving the chain time to observe it.
            let pollFor :: Int -> RunMonad m Bool
                pollFor k
                  | k <= 0 = pure False
                  | otherwise =
                      allClosed >>= \case
                        True -> pure True
                        False -> lift (threadDelay 1) >> pollFor (k - 1)
            pollFor 60 >>= \case
              True -> pure ()
              False -> closeWithRetry (n - 1)
  -- Three attempts a minute apart: the race this covers resolves within a
  -- block or two, and a head that cannot close should fail fast.
  closeWithRetry 3
 where
  isOpen :: NodeState Tx -> Bool
  isOpen st' = case headState st' of
    HeadLogic.Open{} -> True
    _ -> False

performFanout :: (MonadThrow m, MonadAsync m, MonadDelay m) => Party -> RunMonad m UTxO
performFanout party = do
  performStartFanout party
  performObserveFanoutFinalized party

-- | Send 'Fanout' once the head is ready for it, without waiting for the
-- fanout to complete.
performStartFanout :: (MonadSTM m, MonadThrow m, MonadDelay m) => Party -> RunMonad m ()
performStartFanout party = do
  nodes <- gets nodes
  waitForReadyToFanout (nodes ! party)
  party `sendsInput` Input.Fanout

-- | Wait for the head to be finalized on the given party's node and return
-- what the fanout distributed.
performObserveFanoutFinalized :: (MonadSTM m, MonadThrow m, MonadDelay m) => Party -> RunMonad m UTxO
performObserveFanoutFinalized party = do
  nodes <- gets nodes
  -- A fanout may take several partial steps of one block (20s) each, so give
  -- it well over a handful of blocks.
  findInOutput (nodes ! party) (600 :: Int)
 where
  findInOutput :: (MonadDelay m, MonadThrow m) => TestHydraClient Tx m -> Int -> RunMonad m UTxO
  findInOutput node n
    | n == 0 = failure "Failed to perform Fanout"
    | otherwise = do
        outputs <- lift $ serverOutputs node
        case find headIsFinalized outputs of
          Just (HeadIsFinalized{finalizedUTxO}) -> pure finalizedUTxO
          _ -> lift (threadDelay 1) >> findInOutput node (n - 1)

  headIsFinalized :: ServerOutput Tx -> Bool
  headIsFinalized = \case
    HeadIsFinalized{} -> True
    _otherwise -> False

-- | Hand the node a selection to fan out (manual mode) and wait until every
-- node reports all of it distributed: over one or more partial steps, or by
-- the final fanout if the selection drains the head.
performPartialFanoutStep :: (MonadThrow m, MonadTimer m, MonadDelay m) => Party -> UTxOType Payment -> RunMonad m ()
performPartialFanoutStep party selection = do
  nodes <- gets nodes
  waitForReadyToFanout (nodes ! party)
  party `sendsInput` Input.PartialFanout{utxoToFanout = toRealUTxO selection}
  let expected = sortTxOuts (toTxOuts selection)
      distributedSoFar :: [ServerOutput Tx] -> [TxOut CtxUTxO]
      distributedSoFar outs =
        concat
          [ UTxO.txOutputs u
          | out <- outs
          , u <- case out of
              HeadPartiallyFannedOut{distributedUTxO} -> [distributedUTxO]
              HeadIsFinalized{finalizedUTxO} -> [finalizedUTxO]
              _ -> []
          ]
  lift . waitUntilHistory ("partial fanout of " <> show (length selection) <> " outputs") (elems nodes) $ \outs ->
    null (expected \\ sortTxOuts (distributedSoFar outs))

-- | Wait until every node has reported at least @n@ partial fanout steps.
performObservePartialFanoutSteps :: (MonadThrow m, MonadTimer m, MonadDelay m) => Int -> RunMonad m ()
performObservePartialFanoutSteps n = do
  nodes <- gets nodes
  lift . waitForOutputs (show n <> " partial fanout steps") n (elems nodes) $ \case
    HeadPartiallyFannedOut{} -> True
    _ -> False

performCloseWithInitialSnapshot :: (MonadThrow m, MonadTimer m, MonadDelay m, MonadAsync m, MonadLabelledSTM m) => WorldState -> Party -> RunMonad m ()
performCloseWithInitialSnapshot st party = do
  nodes <- gets nodes
  let thisNode = nodes ! party
  waitForOpen thisNode
  case hydraState st of
    Open{} -> do
      SimulatedChainNetwork{closeWithInitialSnapshot} <- gets chain
      lift $ do
        _ <- closeWithInitialSnapshot party
        waitUntilMatch (elems nodes) $ \case
          HeadIsClosed{snapshotNumber} ->
            -- we deliberately wait to see close with the initial snapshot
            -- here to mimic one node not seeing the confirmed tx
            guard $ snapshotNumber == Snapshot.UnsafeSnapshotNumber 0
          _ -> Nothing
    _ -> error "Not in open state"

performRollbackAndForward :: (MonadThrow m, MonadTimer m) => Natural -> RunMonad m ()
performRollbackAndForward numberOfBlocks = do
  SimulatedChainNetwork{rollbackAndForward} <- gets chain
  lift $ rollbackAndForward numberOfBlocks

performRollbackAndFork :: (MonadThrow m, MonadTimer m) => Natural -> RequeueMode -> RunMonad m ()
performRollbackAndFork numberOfBlocks requeueErased = do
  SimulatedChainNetwork{rollbackAndFork} <- gets chain
  lift $ rollbackAndFork numberOfBlocks requeueErased

-- | Crash a node (cancelling its main thread, so any in-flight inputs and
-- in-memory-only state are lost) and start it again from its own event store,
-- re-syncing the chain from genesis. Models a node operator restart /
-- fail-recovery under load: the head must stay live through it.
performRestartNode ::
  ( MonadAsync m
  , MonadLabelledSTM m
  , MonadFork m
  , MonadMask m
  , MonadDelay m
  , MonadTime m
  ) =>
  WorldState ->
  Party ->
  RunMonad m ()
performRestartNode st party = do
  tr <- gets logger
  mockChain <- gets chain
  stores <- gets eventStores
  threadsByParty <- gets nodeThreads
  case (Map.lookup party stores, Map.lookup party threadsByParty, findHsk) of
    (Just eventStore, Just oldThread, Just hsk) -> do
      -- Crash the node: cancel the main thread; the event store survives.
      lift $ cancel oldThread
      let otherParties = filter (/= party) allParties
      (testClient, newThread) <- startNode tr mockChain seedCP eventStore hsk otherParties
      -- The node picks up the network messages it had not consumed before the
      -- crash (see 'connectNode'). They are queued for processing and count as
      -- delivered from then on, like a persisted etcd revision. Let it process
      -- them before the next action: crashing again in the same instant would
      -- lose them for good, and a party that loses an AckSn can never confirm
      -- that snapshot.
      lift $ threadDelay 1
      modify $ \n ->
        n
          { nodes = Map.insert party testClient (nodes n)
          , nodeThreads = Map.insert party newThread (nodeThreads n)
          , threads = newThread : threads n
          }
    _ -> pure ()
 where
  WorldState{hydraParties} = st
  allParties = deriveParty . fst <$> hydraParties
  findHsk = fst <$> find ((== party) . deriveParty . fst) hydraParties
  seedCP = case hydraState st of
    Open{headParameters = HeadParameters{contestationPeriod}} -> contestationPeriod
    _ -> defaultContestationPeriod

stopTheWorld :: MonadAsync m => RunMonad m ()
stopTheWorld =
  gets threads >>= mapM_ (lift . cancel)

-- ** Utility functions

-- | Convert payment-style utxos into transaction outputs.
toTxOuts :: [(CardanoSigningKey, Value)] -> [TxOut CtxUTxO]
toTxOuts payments =
  uncurry mkTxOut <$> payments

-- | Convert payment-style utxos into real utxos. The 'Payment' tx domain is
-- smaller than UTxO and we map every unique signer + value entry to a mocked
-- 'TxIn' on the real cardano domain.
toRealUTxO :: UTxOType Payment -> UTxOType Tx
toRealUTxO paymentUTxO =
  UTxO.fromList $
    [ (mkMockTxIn sk ix, mkTxOut sk val)
    | (sk, vals) <- Map.toList skMap
    , (ix, val) <- zip [0 ..] vals
    ]
 where
  skMap = Map.fromListWith (++) $ map (\(sk, v) -> (sk, [v])) paymentUTxO

mkTxOut :: CardanoSigningKey -> Value -> TxOut CtxUTxO
mkTxOut (CardanoSigningKey sk) val =
  TxOut (mkVkAddress testNetworkId (getVerificationKey sk)) val TxOutDatumNone ReferenceScriptNone

mkMockTxIn :: CardanoSigningKey -> Word -> TxIn
mkMockTxIn (CardanoSigningKey sk) ix =
  TxIn (TxId tid) (TxIx ix)
 where
  vk = getVerificationKey sk
  -- NOTE: Ugly, works because both binary representations are 32-byte long.
  tid = unsafeDeserialize' (serialize' vk)

waitForUTxOToSpend ::
  forall m.
  MonadDelay m =>
  UTxO ->
  CardanoSigningKey ->
  Value ->
  TestHydraClient Tx m ->
  m (Either UTxO (TxIn, TxOut CtxUTxO))
waitForUTxOToSpend utxo key value node = go utxo 100
 where
  -- Reports the head UTxO as last seen when giving up, not the caller's
  -- initial one, so a missing output can be told from an empty head.
  go :: UTxO -> Int -> m (Either UTxO (TxIn, TxOut CtxUTxO))
  go lastSeen = \case
    0 ->
      pure $ Left lastSeen
    n -> do
      u <- headUTxO node
      if u /= mempty
        then case find matchPayment (UTxO.toList u) of
          Nothing -> go u (n - 1)
          Just (txIn, txOut) -> pure $ Right (txIn, txOut)
        else go u (n - 1)

  matchPayment p@(_, txOut) =
    isOwned key p && value == txOutValue txOut

headUTxO ::
  (IsTx tx, MonadDelay m) =>
  TestHydraClient tx m ->
  m (UTxOType tx)
headUTxO node = do
  fromMaybe mempty . getHeadUTxO . headState <$> queryState node

isOwned :: CardanoSigningKey -> (TxIn, TxOut ctx) -> Bool
isOwned (CardanoSigningKey sk) (_, TxOut{txOutAddress = ShelleyAddressInEra (ShelleyAddress _ cre _)}) =
  case fromShelleyPaymentCredential cre of
    (PaymentCredentialByKey ha) -> verificationKeyHash (getVerificationKey sk) == ha
    _ -> False
isOwned _ _ = False

headIsOpen :: ServerOutput tx -> Bool
headIsOpen = \case
  HeadIsOpen{} -> True
  _otherwise -> False

headIsReadyToFanout :: ServerOutput tx -> Bool
headIsReadyToFanout = \case
  ReadyToFanout{} -> True
  _otherwise -> False
