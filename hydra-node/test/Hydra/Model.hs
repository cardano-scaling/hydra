{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# HLINT ignore "Use lambda-case" #-}

-- | A /Model/ of the Hydra head Protocol
--
-- This model integrates in a single state-machine like abstraction the whole behaviour of
-- a Hydra Head, taking into account both on-chain state and contracts, and off-chain
-- interactions. It is written from the point of view of a pre-defined set of Hydra node
-- /operators/ that want to create a channel between them.
module Hydra.Model where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (Any, label)

import qualified Cardano.Api.UTxO as UTxO
import Cardano.Binary (serialize', unsafeDeserialize')
import Cardano.Crypto.Hash (hash)
import Cardano.Crypto.Hash.SHA256 (SHA256)
import Cardano.Ledger.Keys (hashKey)
import Control.Monad (liftM2)
import Control.Monad.Class.MonadAsync (async)
import Control.Monad.Class.MonadSTM (newTQueue, newTVarIO)
import Control.Monad.Class.MonadTimer (timeout)
import Data.List (nub)
import qualified Data.List as List
import Data.Map ((!))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Hydra.BehaviorSpec (
  TestHydraNode (..),
  createHydraNode,
  createTestHydraNode,
  simulatedChainAndNetwork,
  waitUntil,
  waitUntilMatch,
 )
import Hydra.Cardano.Api.Prelude (fromShelleyPaymentCredential)
import Hydra.Chain (HeadParameters (..))
import Hydra.Chain.Direct.Fixture (defaultGlobals, defaultLedgerEnv, testNetworkId)
import Hydra.ClientInput (ClientInput)
import qualified Hydra.ClientInput as Input
import qualified Hydra.Crypto as Hydra
import Hydra.HeadLogic (Committed, PendingCommits)
import Hydra.Ledger (IsTx (..))
import Hydra.Ledger.Cardano (cardanoLedger, genAdaValue, genKeyPair, genSigningKey, mkSimpleTx)
import Hydra.Logging (Tracer)
import Hydra.Node (HydraNodeLog, runHydraNode)
import Hydra.Party (Party, deriveParty)
import Hydra.ServerOutput (ServerOutput (GetUTxOResponse, ReadyToCommit, SnapshotConfirmed))
import qualified Hydra.ServerOutput as Output
import qualified Hydra.Snapshot as Snapshot
import Test.QuickCheck (elements, frequency, listOf1, resize, sized, suchThat, tabulate, vector, vectorOf)
import Test.QuickCheck.StateModel (Any (..), LookUp, StateModel (..), Var)
import qualified Prelude

-- | Local state as seen by each Head participant.
data LocalState
  = Start
  | Idle
      { idleParties :: [Party]
      , cardanoKeys :: [VerificationKey PaymentKey]
      }
  | Initial
      { headParameters :: HeadParameters
      , commits :: Committed Payment
      , pendingCommits :: PendingCommits
      }
  | Open
      { headParameters :: HeadParameters
      , offChainState :: OffChainState
      }
  | Final {finalUTxO :: UTxOType Payment}
  deriving stock (Eq, Show)

isInitialState :: LocalState -> Bool
isInitialState Initial{} = True
isInitialState _ = False

isFinalState :: LocalState -> Bool
isFinalState Final{} = True
isFinalState _ = False

isIdleState :: LocalState -> Bool
isIdleState Idle{} = True
isIdleState _ = False

isPendingCommitFrom :: Party -> LocalState -> Bool
isPendingCommitFrom party Initial{pendingCommits} =
  party `Set.member` pendingCommits
isPendingCommitFrom _ _ = False

data OffChainState = OffChainState
  { confirmedUTxO :: UTxOType Payment
  , seenTransactions :: [Payment]
  }
  deriving stock (Eq, Show)

-- | Global state maintained by the model.
data WorldState (m :: Type -> Type) = WorldState
  { hydraParties :: [(Hydra.SigningKey, CardanoSigningKey)]
  , hydraState :: LocalState
  }
  deriving (Eq, Show)

data Nodes m = Nodes
  { nodes :: Map.Map Party (TestHydraNode Tx m)
  , logger :: Tracer m (HydraNodeLog Tx)
  }

type CardanoSigningKey = SigningKey PaymentKey

instance Eq CardanoSigningKey where
  (PaymentSigningKey skd) == (PaymentSigningKey skd') = skd == skd'

unwrapAddress :: AddressInEra -> Text
unwrapAddress = \case
  ShelleyAddressInEra addr -> serialiseToBech32 addr
  ByronAddressInEra{} -> error "Byron."

data Payment = Payment
  { from :: CardanoSigningKey
  , to :: CardanoSigningKey
  , value :: Value
  }
  deriving (Eq, Generic, ToJSON, FromJSON)

instance Show Payment where
  -- NOTE: We display derived addresses instead of raw signing keys in order to help troubleshooting
  -- tests failures or errors: The UTxO display addresses in bech32 format.
  show Payment{from, to, value} =
    "Payment { from = " <> signingKeyAsAddress from
      <> ", to = "
      <> signingKeyAsAddress to
      <> ", value = "
      <> show value
      <> " }"
   where
    signingKeyAsAddress = show . mkVkAddress @Era testNetworkId . getVerificationKey

applyTx :: UTxOType Payment -> Payment -> UTxOType Payment
applyTx utxo Payment{from, to, value} =
  (to, value) : List.delete (from, value) utxo

instance ToJSON (CardanoSigningKey) where
  toJSON = error "don't use"

instance FromJSON (CardanoSigningKey) where
  parseJSON = error "don't use"

instance Arbitrary Payment where
  arbitrary = error "don't use"

instance Arbitrary Value where
  arbitrary = genAdaValue

instance Arbitrary (CardanoSigningKey) where
  arbitrary = snd <$> genKeyPair

instance ToCBOR Payment where
  toCBOR = error "don't use"

instance FromCBOR Payment where
  fromCBOR = error "don't use"

instance IsTx Payment where
  type TxIdType Payment = Int
  type UTxOType Payment = [(CardanoSigningKey, Value)]
  type ValueType Payment = Value
  txId = error "undefined"
  balance = foldMap snd
  hashUTxO = encodeUtf8 . show @Text

instance
  ( MonadSTM m
  , MonadDelay m
  , MonadAsync m
  , MonadCatch m
  , MonadTime m
  , MonadTimer m
  , MonadFork m
  ) =>
  StateModel (WorldState m)
  where
  data Action (WorldState m) a where
    Seed ::
      { seedKeys :: [(Hydra.SigningKey, CardanoSigningKey)]
      } ->
      Action (WorldState m) ()
    Command ::
      { party :: Party
      , command :: ClientInput Payment
      } ->
      Action (WorldState m) ()

  type ActionMonad (WorldState m) = StateT (Nodes m) m

  actionName :: Action (WorldState m) a -> String
  actionName Command{command} = unsafeConstructorName command
  actionName Seed{} = "Seed"

  initialState =
    WorldState
      { hydraParties = mempty
      , hydraState = Start
      }

  arbitraryAction :: WorldState m -> Gen (Any (Action (WorldState m)))
  arbitraryAction st@WorldState{hydraParties, hydraState} =
    case hydraState of
      Start -> genSeed
      Idle{} -> genInit
      Initial{pendingCommits} ->
        frequency
          [ (5, genCommit pendingCommits)
          , (1, genAbort)
          ]
      Open{} ->
        frequency
          [ (10, Some <$> genNewTx st)
          ]
      _ -> genSeed
   where
    genSeed = Some . Seed <$> resize 7 partyKeys

    genInit = do
      initContestationPeriod <- arbitrary
      key <- fst <$> elements hydraParties
      let command = Input.Init{Input.contestationPeriod = initContestationPeriod}
      pure $ Some Command{party = deriveParty key, command}

    genCommit pending = do
      party <- elements $ toList pending
      let (_, sk) = fromJust $ find ((== party) . deriveParty . fst) hydraParties
      value <- genAdaValue
      let command = Input.Commit{Input.utxo = [(sk, value)]}
      pure $ Some Command{party, command}

    genAbort = do
      (key, _) <- elements hydraParties
      pure $ Some Command{party = deriveParty key, command = Input.Abort}

  precondition WorldState{hydraState = Start} Seed{} =
    True
  precondition WorldState{hydraState = Idle{}} Command{command = Input.Init{}} =
    True
  precondition WorldState{hydraState = hydraState@Initial{}} Command{party, command = Input.Commit{}} =
    isPendingCommitFrom party hydraState
  precondition WorldState{hydraState = Initial{}} Command{command = Input.Abort{}} =
    True
  precondition WorldState{hydraState = Open{offChainState}} Command{command = Input.NewTx{Input.transaction = tx}} =
    case List.lookup (from tx) (confirmedUTxO offChainState) of
      Just v -> v == value tx
      Nothing -> False
  precondition _ _ =
    False

  nextState :: WorldState m -> Action (WorldState m) a -> Var a -> WorldState m
  nextState _ Seed{seedKeys} _ =
    WorldState{hydraParties = seedKeys, hydraState = Idle{idleParties, cardanoKeys}}
   where
    idleParties = map (deriveParty . fst) seedKeys
    cardanoKeys = map (getVerificationKey . snd) seedKeys
  nextState WorldState{hydraParties, hydraState} Command{command = Input.Init{Input.contestationPeriod}} _ =
    WorldState{hydraParties, hydraState = mkInitialState hydraState}
   where
    mkInitialState = \case
      Idle{idleParties} ->
        Initial
          { headParameters =
              HeadParameters
                { parties = idleParties
                , contestationPeriod
                }
          , commits = mempty
          , pendingCommits = Set.fromList idleParties
          }
      _ -> error "unexpected state"
  nextState WorldState{hydraParties, hydraState} Command{party, command = Input.Commit{Input.utxo}} _ =
    WorldState{hydraParties, hydraState = updateWithCommit hydraState}
   where
    updateWithCommit = \case
      Initial{headParameters, commits, pendingCommits} -> updatedState
       where
        commits' = Map.insert party utxo commits
        pendingCommits' = party `Set.delete` pendingCommits
        updatedState =
          if null pendingCommits'
            then
              Open
                { headParameters
                , offChainState =
                    OffChainState
                      { confirmedUTxO = mconcat (Map.elems commits')
                      , seenTransactions = []
                      }
                }
            else
              Initial
                { headParameters
                , commits = commits'
                , pendingCommits = pendingCommits'
                }
      _ -> error "unexpected state"
  nextState WorldState{hydraParties, hydraState} Command{command = Input.Abort} _ =
    WorldState{hydraParties, hydraState = updateWithAbort hydraState}
   where
    updateWithAbort = \case
      Initial{commits} -> Final committedUTxO
       where
        committedUTxO = mconcat $ Map.elems commits
      _ -> Final mempty
  nextState WorldState{hydraParties, hydraState} Command{command = Input.NewTx{Input.transaction = tx}} _ =
    WorldState{hydraParties, hydraState = updateWithNewTx hydraState}
   where
    updateWithNewTx = \case
      Open{headParameters, offChainState = OffChainState{confirmedUTxO, seenTransactions}} ->
        Open
          { headParameters
          , offChainState =
              OffChainState
                { confirmedUTxO = confirmedUTxO `applyTx` tx
                , seenTransactions = tx : seenTransactions
                }
          }
      _ -> error "unexpected state"
  nextState _ _ _ = error "not implemented"

  perform :: WorldState m -> Action (WorldState m) a -> LookUp -> StateT (Nodes m) m a
  perform _ Seed{seedKeys} _ = do
    let parties = map (deriveParty . fst) seedKeys
    tr <- gets logger
    nodes <- lift $ do
      let ledger = cardanoLedger defaultGlobals defaultLedgerEnv
      connectToChain <- simulatedChainAndNetwork
      forM seedKeys $ \(sk, _csk) -> do
        outputs <- atomically newTQueue
        outputHistory <- newTVarIO []
        let party = deriveParty sk
            otherParties = filter (/= party) parties
        node <- createHydraNode ledger sk otherParties outputs outputHistory connectToChain
        let testNode = createTestHydraNode outputs outputHistory node connectToChain
        void $ async $ runHydraNode tr node
        pure (party, testNode)

    modify $ \n -> n{nodes = Map.fromList nodes}
  perform st Command{party, command} _ = do
    case command of
      Input.Commit{Input.utxo = utxo} -> do
        nodes <- gets nodes
        case Map.lookup party nodes of
          Nothing -> error $ "unexpected party " <> Hydra.Prelude.show party
          Just actorNode -> do
            lift $ waitUntil [actorNode] $ ReadyToCommit (Set.fromList $ Map.keys nodes)
            let realUtxo =
                  UTxO.fromPairs $
                    [ (mkMockTxIn vk ix, txOut)
                    | (ix, (sk, val)) <- zip [0 ..] utxo
                    , let vk = getVerificationKey sk
                    , let txOut = TxOut (mkVkAddress testNetworkId vk) val TxOutDatumNone
                    ]
            party `performs` Input.Commit{Input.utxo = realUtxo}
      Input.NewTx{Input.transaction = tx} -> do
        let recipient = mkVkAddress testNetworkId (getVerificationKey (to tx))
        nodes <- gets nodes

        let waitForOpen = do
              outs <- lift $ serverOutputs (nodes ! party)
              let matchHeadIsOpen = \case
                    Output.HeadIsOpen{} -> True
                    _ -> False
              case find matchHeadIsOpen outs of
                Nothing -> lift (threadDelay 0.1) >> waitForOpen
                Just{} -> pure ()
        waitForOpen

        party `performs` Input.GetUTxO

        let matchPayment p@(_, txOut) =
              isOwned (from tx) p && value tx == txOutValue txOut

            waitForUTxO utxo = \case
              0 ->
                lift $
                  throwIO $
                    Prelude.userError
                      ( "no utxo matched,\npayment = " <> show tx
                          <> "\nutxo =  "
                          <> show utxo
                          <> "\nconfirmed = "
                          <> show (fmap (first (mkVkAddress @Era testNetworkId . getVerificationKey)) $ confirmedUTxO $ offChainState $ hydraState st)
                      )
              n ->
                lift (threadDelay 1 >> waitForNext (nodes ! party)) >>= \case
                  GetUTxOResponse u
                    | u == mempty -> do
                      party `performs` Input.GetUTxO
                      waitForUTxO u (n -1)
                    | otherwise -> case find matchPayment (UTxO.pairs u) of
                      Nothing -> do
                        party `performs` Input.GetUTxO
                        waitForUTxO u (n -1)
                      Just p -> pure p
                  _ ->
                    waitForUTxO utxo (n -1)

        (i, o) <- waitForUTxO mempty (5000 :: Int)

        let realTx =
              either
                (error . show)
                id
                (mkSimpleTx (i, o) (recipient, value tx) (from tx))

        party `performs` Input.NewTx realTx
        lift $
          waitUntilMatch [nodes ! party] $ \case
            SnapshotConfirmed{Output.snapshot = snapshot} ->
              realTx `elem` Snapshot.confirmed snapshot
            err@Output.TxInvalid{} -> error ("expected tx to be valid: " <> show err)
            _ -> False
      Input.Init{Input.contestationPeriod = p} -> do
        party `performs` Input.Init{Input.contestationPeriod = p}
      Input.Abort -> do
        party `performs` Input.Abort
      Input.GetUTxO -> do
        party `performs` Input.GetUTxO
      Input.Close -> do
        party `performs` Input.Close
      Input.Contest -> do
        party `performs` Input.Contest
      Input.Fanout -> do
        party `performs` Input.Fanout

  monitoring (s, s') _action _lookup _return =
    case (hydraState s, hydraState s') of
      (st, st') -> tabulate "Transitions" [unsafeConstructorName st <> " -> " <> unsafeConstructorName st']

genNewTx :: WorldState m -> Gen (Action (WorldState m) ())
genNewTx WorldState{hydraParties, hydraState} =
  case hydraState of
    Open{offChainState = OffChainState{confirmedUTxO}} -> do
      (from, value) <-
        elements confirmedUTxO `suchThat` (not . null . valueToList . snd)
      let party = deriveParty $ fst $ fromJust $ List.find ((== from) . snd) hydraParties
      (_, to) <- elements hydraParties `suchThat` ((/= from) . snd)
      pure $ Command{party, command = Input.NewTx{Input.transaction = Payment{from, to, value}}}
    _ -> error $ "genNewTx impossible in state: " <> show hydraState

unsafeConstructorName :: (Show a) => a -> String
unsafeConstructorName = Prelude.head . Prelude.words . show

performs :: Monad m => Party -> ClientInput Tx -> StateT (Nodes m) m ()
performs party command = do
  nodes <- gets nodes
  case Map.lookup party nodes of
    Nothing -> error $ "unexpected party " <> Hydra.Prelude.show party
    Just actorNode -> lift $ actorNode `send` command

-- |Generate a list of pairs of Hydra/Cardano signing keys.
-- All the keys in this list are guaranteed to be unique.
partyKeys :: Gen [(Hydra.SigningKey, CardanoSigningKey)]
partyKeys =
  sized $ \len -> do
    hks <- nub <$> vectorOf len arbitrary
    cks <- nub <$> vectorOf len genSigningKey
    pure $ zip hks cks

isOwned :: CardanoSigningKey -> (TxIn, TxOut ctx) -> Bool
isOwned sk (_, TxOut (ShelleyAddressInEra (ShelleyAddress _ cre _)) _ _) =
  case fromShelleyPaymentCredential cre of
    (PaymentCredentialByKey ha) -> verificationKeyHash (getVerificationKey sk) == ha
    _ -> False
isOwned _ _ = False

mkMockTxIn :: VerificationKey PaymentKey -> Word -> TxIn
mkMockTxIn vk ix = TxIn (TxId tid) (TxIx ix)
 where
  -- NOTE: Ugly, works because both binary representations are 32-byte long.
  tid = unsafeDeserialize' (serialize' vk)

deriving instance Show (Action (WorldState m) a)
deriving instance Eq (Action (WorldState m) a)
