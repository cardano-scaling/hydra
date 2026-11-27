{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Playground.Engine where

import Hydra.Prelude hiding (encodeUtf8, state)

import Data.Aeson (Value, object, withObject, (.!=), (.:), (.:?), (.=))
import Data.Foldable (minimum)
import Data.List (isPrefixOf)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.API.ClientInput qualified as Client
import Hydra.Chain qualified as Chain
import Hydra.Chain.ChainState (ChainSlot)
import Hydra.HeadLogic.Cardano (cardanoSnapshotLimits)
import Hydra.HeadLogic.Input (Input (..))
import Hydra.HeadLogic.Interpreter qualified as Interpreter
import Hydra.HeadLogic.Outcome qualified as Logic
import Hydra.HeadLogic.Retry qualified as Retry
import Hydra.HeadLogic.Rules.Chain.Deposits (depositActivationTime)
import Hydra.HeadLogic.Rules.Environment (RuleEnvironment (..))
import Hydra.HeadLogic.Rules.Input qualified as Input
import Hydra.HeadLogic.Snapshot.Settlement (settledUTxO)
import Hydra.HeadLogic.State qualified as Head
import Hydra.HeadLogic.State.Node qualified as Machine
import Hydra.HeadLogic.Trace qualified as Trace
import Hydra.HeadLogic.Workflow qualified as Workflow
import Hydra.Ledger.Simple (SimpleChainState (SimpleChainState), SimpleTx (..), SimpleTxOut (..), simpleLedger)
import Hydra.Network.Message qualified as Network
import Hydra.Node.Input (mkNetworkInput)
import Hydra.Node.State qualified as Node
import Hydra.Node.UnsyncedPeriod (defaultUnsyncedPeriodFor)
import Hydra.Tx.Crypto qualified as Crypto
import Hydra.Tx.DepositPeriod qualified as Period
import Hydra.Tx.HeadId (HeadId (..), HeadSeed (..))
import Hydra.Tx.OnChainId (OnChainId (..))
import Hydra.Tx.Party (Party (..))
import Hydra.Tx.Snapshot qualified as Snapshot

data Setup = Setup {operators :: Int, users :: Int, connections :: [Int]}
  deriving stock (Eq, Show, Generic)

instance FromJSON Setup where
  parseJSON = withObject "setup" $ \o -> Setup <$> o .: "operators" <*> o .: "users" <*> o .:? "connections" .!= []

data Action
  = Open
  | Step
  | Move {fund :: Text, owner :: Text, layer :: Text}
  | Pay {funds :: [Text], owner :: Text}
  | Submit {recipientNode :: Int, clientCommand :: Client.ClientInput SimpleTx}
  deriving stock (Eq, Show)

instance FromJSON Action where
  parseJSON = withObject "action" $ \o ->
    o .: "type" >>= \case
      ("open" :: Text) -> pure Open
      "step" -> pure Step
      "move" -> Move <$> o .: "fund" <*> o .: "owner" <*> o .: "layer"
      "pay" -> Pay <$> o .: "funds" <*> o .: "owner"
      "client" -> Submit <$> o .: "node" <*> o .: "input"
      _ -> fail "Unknown playground action"

data Request = Request {setup :: Setup, actions :: [Action], audit :: Bool}

instance FromJSON Request where
  parseJSON = withObject "playground request" $ \o -> Request <$> o .: "setup" <*> o .:? "actions" .!= [] <*> o .:? "audit" .!= False

data Person = Person {personId :: Text, name :: Text, operator :: Int, runsNode :: Bool}
  deriving stock (Eq, Show)

data Chip = Chip {chipId :: Text, ownerId :: Text}
  deriving stock (Eq, Show)

data Delivery
  = Broadcast Int (Network.Message SimpleTx)
  | RetryDelivery Int (Input SimpleTx)
  | Posted (Chain.PostChainTx SimpleTx)
  | WalletDeposit Integer SimpleTxOut
  deriving stock (Eq, Show)

data Execution = Execution
  { operatorIndex :: Int
  , input :: Input SimpleTx
  , outcome :: Logic.Outcome SimpleTx
  , decisions :: [Trace.TraceStep]
  , after :: Node.NodeState SimpleTx
  }

data World = World
  { people :: [Person]
  , nodes :: [Node.NodeState SimpleTx]
  , queue :: [Delivery]
  , wallets :: Set SimpleTxOut
  , l1Deposits :: Map Integer (Set SimpleTxOut)
  , backing :: Int
  , chips :: Map SimpleTxOut Chip
  , nextId :: Integer
  , now :: UTCTime
  , slot :: ChainSlot
  , executions :: [Execution]
  , messages :: [Value]
  , actionStart :: Int
  }

partyFor :: Int -> Party
partyFor = Party . Interpreter.signingIdentity . signerFor

signerFor :: Int -> Interpreter.SnapshotSigner SimpleTx
signerFor i = fromMaybe (error "invalid operator identity") $ viaNonEmpty head $ drop i signers

signers :: [Interpreter.SnapshotSigner SimpleTx]
signers =
  [ Interpreter.SnapshotSigner{signOne = Crypto.sign key, signingIdentity = Crypto.getVerificationKey key}
  | i <- [0 .. 3]
  , let key = Crypto.generateSigningKey $ keySeed i
  ]

keySeed :: Int -> ByteString
keySeed i = encodeUtf8 $ "hydra-playground-operator-" <> show i

operatorIndices :: World -> [Int]
operatorIndices w = [0 .. length w.nodes - 1]

environment :: World -> Int -> RuleEnvironment
environment w i =
  RuleEnvironment
    { snapshotLimits = cardanoSnapshotLimits
    , party = partyFor i
    , otherParties = [partyFor j | j <- operatorIndices w, j /= i]
    , participants = [UnsafeOnChainId $ encodeUtf8 $ show j | j <- operatorIndices w]
    , contestationPeriod = 60
    , depositPeriod = Period.DepositPeriod 60
    , depositActivation = Period.DepositPeriod 3
    , unsyncedPeriod = defaultUnsyncedPeriodFor 60
    , configuredPeers = "playground"
    }

headId :: HeadId
headId = UnsafeHeadId "playground-head"

initial :: Setup -> Either Text World
initial Setup{..}
  | operators < 1 || operators > 4 || users < 0 || users > 4 = Left "Choose 1–4 operators and 0–4 wallet users."
  | not (null connections) && (length connections /= users || any (\i -> i < 0 || i >= operators) connections) = Left "Each wallet user needs an operator connection."
  | otherwise = Right $ foldl' (\w i -> deliver i (ChainInput $ Chain.Tick w.now w.slot) w) world [0 .. operators - 1]
 where
  routes = if null connections then [i `mod` operators | i <- [0 .. users - 1]] else connections
  people =
    [Person ("operator-" <> show i) n i True | (i, n) <- zip [0 ..] $ take operators ["Alice", "Bob", "Carol", "Devon"]]
      <> [Person ("user-" <> show i) n op False | (i, (n, op)) <- zip [0 :: Int ..] $ zip (take users ["Erin", "Finn", "Grace", "Hugo"]) routes]
  entries = [(SimpleTxOut $ fromIntegral $ i * 2 + j + 1, Chip ("coin-" <> show (i * 2 + j)) p.personId) | (i, p) <- zip [0 :: Int ..] people, j <- [0, 1]]
  world =
    World
      { people
      , nodes = replicate operators (Node.initNodeState $ SimpleChainState 0)
      , queue = []
      , wallets = Set.fromList $ fst <$> entries
      , l1Deposits = mempty
      , backing = 0
      , chips = Map.fromList entries
      , nextId = 1000
      , now = posixSecondsToUTCTime 0
      , slot = 0
      , executions = []
      , messages = []
      , actionStart = 0
      }

evaluateInput :: World -> Int -> Input SimpleTx -> (Logic.Outcome SimpleTx, [Trace.TraceStep])
evaluateInput w i input =
  Interpreter.traceProgram config $
    Workflow.run $
      Input.workflow Input.Args{env = environment w i, nodeState = nodeAt w i, now = w.now, input}
 where
  config = Interpreter.mkInterpreterConfig simpleLedger $ signerFor i

nodeAt :: World -> Int -> Node.NodeState SimpleTx
nodeAt w i = fromMaybe (error "invalid operator index") $ viaNonEmpty head $ drop i w.nodes

deliver :: Int -> Input SimpleTx -> World -> World
deliver i input w = foldl' (acceptEffect i) notified effects
 where
  (outcome, decisions) = evaluateInput w i input
  changes = case outcome of
    Logic.Continue{stateChanges} -> stateChanges
    Logic.Wait{stateChanges} -> stateChanges
    Logic.Error{} -> []
  effects = case outcome of
    Logic.Continue{effects = emitted} -> emitted
    _ -> []
  after = Machine.applyEvents (nodeAt w i) changes
  updated =
    w
      { nodes = [if j == i then after else n | (j, n) <- zip [0 ..] w.nodes]
      , executions = w.executions <> [Execution{operatorIndex = i, input, outcome, decisions, after}]
      }
  withRetry = case outcome of
    Logic.Wait{reason} -> case Retry.retryDecision reason input of
      Retry.RetryInput retry -> updated{queue = updated.queue <> [RetryDelivery i retry]}
      Retry.ParkUntilSynced -> updated{queue = updated.queue <> [RetryDelivery i input]}
      Retry.DropInput -> updated
    _ -> updated
  notified = foldl' notifyClient withRetry changes
  notifyClient world Logic.SnapshotConfirmed{} = say (nodeName w i) "Connected wallets" "All operators’ signatures match. This node confirms the snapshot." "SnapshotConfirmed" world
  notifyClient world Logic.TxInvalid{} = say (nodeName w i) "Connected wallet" "The transaction failed SimpleTx validation; funds are unchanged." "TxInvalid" world
  notifyClient world Logic.DecommitInvalid{} = say (nodeName w i) "Connected wallet" "The protocol declined this withdrawal. See the exact reason below." "DecommitInvalid" world
  notifyClient world _ = world

acceptEffect :: Int -> World -> Logic.Effect SimpleTx -> World
acceptEffect i w effect = case effect of
  Logic.NetworkEffect message ->
    say (nodeName w i) "All operators (including sender)" (networkExplanation message) (messageName message) $
      w{queue = w.queue <> [Broadcast i message]}
  Logic.OnChainEffect post ->
    say (nodeName w i) "Cardano L1" "Submit the transaction produced by the protocol." (postName post) $
      w{queue = if Posted post `elem` w.queue then w.queue else w.queue <> [Posted post]}
  Logic.ClientEffect{} -> say (nodeName w i) "Connected client" "The node declined this client request. Inspect its response below." "ClientEffect" w

say :: Text -> Text -> Text -> Text -> World -> World
say from to text api w = w{messages = w.messages <> [object ["from" .= from, "to" .= to, "text" .= text, "api" .= api]]}

nodeName :: World -> Int -> Text
nodeName w i = maybe "Unknown node" (\p -> p.name <> "’s node") $ find (\p -> p.runsNode && p.operator == i) w.people

messageName :: Network.Message SimpleTx -> Text
messageName = \case
  Network.ReqTx{} -> "ReqTx"
  Network.ReqSn{} -> "ReqSn"
  Network.AckSn{} -> "AckSn"
  Network.ReqDec{} -> "ReqDec"

networkExplanation :: Network.Message SimpleTx -> Text
networkExplanation = \case
  Network.ReqTx{} -> "Share the wallet’s transaction for validation on L2."
  Network.ReqSn{snapshotNumber} -> "Propose snapshot " <> show snapshotNumber <> ". Each operator checks the proposal."
  Network.AckSn{snapshotNumber} -> "Send this operator’s signature for snapshot " <> show snapshotNumber <> "."
  Network.ReqDec{} -> "Share the requested withdrawal for validation."

postName :: Chain.PostChainTx SimpleTx -> Text
postName =
  (<> "Tx") . \case
    Chain.InitTx{} -> "Init"
    Chain.IncrementTx{} -> "Increment"
    Chain.DecrementTx{} -> "Decrement"
    other -> Chain.postChainTxName other

openState :: Node.NodeState SimpleTx -> Maybe (Head.OpenState SimpleTx)
openState n = case n.headState of Head.Open s -> Just s; _ -> Nothing

confirmedFunds :: Node.NodeState SimpleTx -> Set SimpleTxOut
confirmedFunds n = maybe mempty (\o -> settledUTxO o.coordinatedHeadState.version $ Snapshot.getSnapshot o.coordinatedHeadState.confirmedSnapshot) $ openState n

localFunds :: Node.NodeState SimpleTx -> Set SimpleTxOut
localFunds n = maybe mempty (.coordinatedHeadState.localUTxO) $ openState n

activationTime :: World -> Maybe UTCTime
activationTime w =
  viaNonEmpty
    minimum
    [ addUTCTime 1 $ depositActivationTime (environment w 0) deposit
    | deposit <- Map.elems $ Node.pendingDeposits $ nodeAt w 0
    , deposit.status == Node.Inactive
    ]

busy :: World -> Bool
busy w = not (null w.queue) || isJust (activationTime w)

runAction :: World -> Action -> Either Text World
runAction w Open
  | busy w = Left "Finish the pending delivery first."
  | all (isNothing . openState) w.nodes =
      Right $
        deliver 0 (ClientInput Client.Init) $
          say "Alice’s client" (nodeName w 0) "Ask to open a Head with the configured operators." "Init" w
  | otherwise = Left "This Head is already open."
runAction w Step = case w.queue of
  delivery : rest -> deliverNext delivery w{queue = rest}
  [] -> case activationTime w of
    Just next -> Right $ tick next $ say "Cardano L1 clock" "All operators" "Advance simulated chain time past the deposit activation delay." "Tick" w
    Nothing -> Left "There is no pending delivery."
runAction w Submit{recipientNode, clientCommand}
  | recipientNode `elem` operatorIndices w = Right $ deliver recipientNode (ClientInput clientCommand) w
  | otherwise = Left "Unknown operator."
runAction w Pay{funds, owner} = transfer w funds owner "l2"
runAction w Move{..} = do
  readyToMove w
  (output, chip, onL1) <- requireChip w fund
  if onL1
    then do
      unless (layer == "l2" && owner == chip.ownerId) $ Left "A deposit keeps its owner."
      source <- requirePerson w chip.ownerId
      pure $
        say (source.name <> "’s wallet") "Cardano L1" "Inspect and submit the deposit. Funds leave the wallet when L1 confirms it." "Deposit submitted" $
          say (nodeName w source.operator) (source.name <> "’s wallet") "Return a deposit draft for the wallet to inspect and sign. This transaction construction is simulated." "Deposit draft" $
            say (source.name <> "’s wallet") (nodeName w source.operator) "Ask the connected operator for a deposit transaction." "POST /commit" $
              w{queue = [WalletDeposit w.nextId output], nextId = w.nextId + 1}
    else transfer w [fund] owner layer

readyToMove :: World -> Either Text ()
readyToMove w = do
  when (busy w) $ Left "Finish this request before moving more funds."
  unless (all (isJust . openState) w.nodes) $ Left "Open the Head first."

requirePerson :: World -> Text -> Either Text Person
requirePerson w owner = maybe (Left "Unknown wallet owner.") Right $ find ((== owner) . personId) w.people

requireChip :: World -> Text -> Either Text (SimpleTxOut, Chip, Bool)
requireChip w fund = maybe (Left "This chip is not available.") Right $ availableChip w fund

transaction :: World -> [SimpleTxOut] -> SimpleTx
transaction w inputs =
  SimpleTx w.nextId (Set.fromList inputs) $
    Set.fromList [SimpleTxOut $ w.nextId + 10000 + i | i <- [0 .. fromIntegral (length inputs) - 1]]

transfer :: World -> [Text] -> Text -> Text -> Either Text World
transfer w funds owner layer = do
  readyToMove w
  when (Set.size (Set.fromList funds) /= length funds) $ Left "Choose each output only once."
  inputs <- traverse (requireChip w) funds
  (_, firstChip, _) <- viaNonEmpty head inputs ?> "Choose at least one output."
  recipient <- requirePerson w owner
  source <- requirePerson w firstChip.ownerId
  unless (all (\(_, chip, onL1) -> not onL1 && chip.ownerId == source.personId) inputs) $ Left "Choose available L2 funds from one wallet."
  unless ((layer == "l1" && owner == source.personId) || (layer == "l2" && owner /= source.personId)) $ Left "Choose another L2 owner or the same owner’s L1 wallet."
  let tx = transaction w [out | (out, _, _) <- inputs]
      produced = zip (toList tx.txOutputs) [Chip chip.chipId owner | (_, chip, _) <- inputs]
      prepared = w{nextId = w.nextId + fromIntegral (length inputs), chips = Map.fromList produced <> w.chips}
      (request, name, detail) =
        if layer == "l1"
          then (Client.Decommit tx, "Decommit", "Request a withdrawal to this wallet on L1.")
          else (Client.NewTx tx, "NewTx", "Submit a payment to " <> recipient.name <> " on L2.")
  pure $ deliver source.operator (ClientInput request) $ say (source.name <> "’s wallet") (nodeName w source.operator) detail name prepared

availableChip :: World -> Text -> Maybe (SimpleTxOut, Chip, Bool)
availableChip w requestedId =
  find (\(_, chip, _) -> chip.chipId == requestedId) $
    [(out, chip, True) | out <- toList w.wallets, Just chip <- [Map.lookup out w.chips]]
      <> [(out, chip, False) | out <- toList agreed, Just chip <- [Map.lookup out w.chips]]
 where
  available n = confirmedFunds n `Set.intersection` localFunds n
  agreed = foldl' Set.intersection (available $ nodeAt w 0) $ available <$> w.nodes

deliverNext :: Delivery -> World -> Either Text World
deliverNext delivery w = case delivery of
  Broadcast sender message -> Right $ foldl' (\world i -> deliver i input world) announced $ operatorIndices w
   where
    input = mkNetworkInput (partyFor sender) message
    announced = say (nodeName w sender) "All operators (including sender)" "Deliver this message; each node runs its own protocol machine." (messageName message) w
  RetryDelivery recipient input -> Right $ deliver recipient input w
  WalletDeposit depositId output ->
    Right $
      observe observation $
        w{wallets = Set.delete output w.wallets, l1Deposits = Map.insert depositId (Set.singleton output) w.l1Deposits}
   where
    observation = Chain.OnDepositTx headId depositId (Set.singleton output) w.now (addUTCTime 600 w.now)
  Posted post -> case post of
    Chain.InitTx{participants, headParameters} -> Right $ observe (Chain.OnInitTx headId (UnsafeHeadSeed "playground-seed") headParameters participants) w
    Chain.IncrementTx{headId = hid, incrementingSnapshot = confirmed, depositTxId} -> do
      deposited <- maybe (Left "L1 cannot increment a deposit that is not present.") Right $ Map.lookup depositTxId w.l1Deposits
      let snapshot = Snapshot.getSnapshot confirmed
      pure $
        observe (Chain.OnIncrementTx hid (snapshot.version + 1) depositTxId snapshot.number (Snapshot.confirmedSignatures confirmed)) $
          w{l1Deposits = Map.delete depositTxId w.l1Deposits, backing = w.backing + Set.size deposited}
    Chain.DecrementTx{headId = hid, decrementingSnapshot = confirmed} -> do
      let snapshot = Snapshot.getSnapshot confirmed
      distributed <- maybe (Left "The posted snapshot has no withdrawal outputs.") Right snapshot.utxoToDecommit
      pure $
        observe (Chain.OnDecrementTx hid (snapshot.version + 1) distributed snapshot.number (Snapshot.confirmedSignatures confirmed)) $
          w{wallets = w.wallets <> distributed, backing = w.backing - Set.size distributed}
    _ -> Left "This walkthrough does not expose closing or recovery."

observe :: Chain.OnChainTx SimpleTx -> World -> World
observe observation w = foldl' (\world i -> deliver i input world) announced $ operatorIndices w
 where
  input = ChainInput $ Chain.Observation observation (SimpleChainState $ w.slot + 1)
  (name, detail) = case observation of
    Chain.OnInitTx{} -> ("OnInitTx", "Init is confirmed. Each configured operator checks the parameters and opens the empty Head.")
    Chain.OnDepositTx{} -> ("OnDepositTx", "The deposit is now locked on L1. Operators record it as inactive.")
    Chain.OnIncrementTx{} -> ("OnIncrementTx", "The increment is confirmed. Operators apply the new Head version and make the deposit available on L2.")
    Chain.OnDecrementTx{} -> ("OnDecrementTx", "The decrement is confirmed. Withdrawal outputs are now in the L1 wallet.")
    _ -> ("Observation", "Observe an L1 transaction.")
  announced = say "Cardano L1" "All operators" detail name w{slot = w.slot + 1}

tick :: UTCTime -> World -> World
tick next w = foldl' (\world i -> deliver i (ChainInput $ Chain.Tick next nextSlot) world) w{now = next, slot = nextSlot} $ operatorIndices w
 where
  nextSlot = w.slot + fromIntegral (max 1 $ ceiling $ diffUTCTime next w.now :: Integer)

replayFrom :: Maybe (Request, World) -> Request -> Either Text World
replayFrom cached Request{setup, actions}
  | length actions > 256 = Left "This walkthrough is limited to 256 steps. Start over to explore another flow."
  | Just (previous, world) <- cached
  , setup == previous.setup
  , previous.actions `isPrefixOf` actions =
      continue world $ drop (length previous.actions) actions
  | otherwise = initial setup >>= \world -> continue world actions
 where
  continue = foldlM (\current -> runAction current{actionStart = length current.executions})
