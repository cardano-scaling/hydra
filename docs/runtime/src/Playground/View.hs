{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Playground.View (render) where

import Hydra.Prelude hiding (local, state)

import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Foldable (maximum)
import Data.List (nubBy)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Hydra.API.ClientInput qualified as Client
import Hydra.Chain qualified as Chain
import Hydra.HeadLogic.Input (Input (ClientInput))
import Hydra.HeadLogic.Outcome qualified as Logic
import Hydra.HeadLogic.Snapshot.Cached qualified as Cached
import Hydra.HeadLogic.Snapshot.Request (isLeader)
import Hydra.HeadLogic.Snapshot.Settlement (settlingDecommit)
import Hydra.HeadLogic.State qualified as Head
import Hydra.HeadLogic.State.Node qualified as Machine
import Hydra.HeadLogic.Trace qualified as Trace
import Hydra.Ledger.Simple
import Hydra.Network.Message qualified as Network
import Hydra.Node.State qualified as Node
import Hydra.Tx.Party (Party (..))
import Hydra.Tx.Snapshot qualified as Snapshot
import Playground.Engine

render :: Bool -> World -> Value
render includeAudit w =
  object
    [ "status" .= ("ok" :: Text)
    , "phase" .= phase
    , "pending" .= if busy w then Just (object ["kind" .= ("delivery" :: Text)]) else Nothing
    , "canClose" .= canPost w Client.Close
    , "canFanout" .= canFanout w
    , "people" .= map personView w.people
    , "funds" .= map (fundView w) visible
    , "snapshot" .= maybe 0 (Snapshot.number . Snapshot.getSnapshot . (.coordinatedHeadState.confirmedSnapshot)) (openState $ nodeAt w 0)
    , "signing" .= signingView w
    , "messages" .= w.messages
    , "totals"
        .= object
          [ "wallets" .= adaValue w w.wallets
          , "deposits" .= adaValue w (fold w.l1Deposits)
          , "backing" .= w.backing
          , "spendable" .= amount (.ada) ["l2"]
          , "pending" .= amount (.ada) ["payment", "withdrawal"]
          , "closed" .= amount (.ada) ["closed"]
          , "tokens"
              .= object
                [ "wallets" .= amount (.tokens) ["l1"]
                , "deposits" .= amount (.tokens) ["deposit"]
                , "backing" .= amount (.tokens) ["l2", "payment", "withdrawal", "closed"]
                , "spendable" .= amount (.tokens) ["l2"]
                , "pending" .= amount (.tokens) ["payment", "withdrawal"]
                , "closed" .= amount (.tokens) ["closed"]
                ]
          ]
    , "action" .= guidance w
    , "targets" .= object [Key.fromText chip.chipId .= destinations w chip.chipId | (_, chip, _) <- visible]
    , "nodes" .= map (nodeView w) (operatorIndices w)
    , "decisions" .= map (executionView w) (drop w.actionStart w.executions)
    , "audit" .= if includeAudit then map (executionView w) w.executions else []
    ]
 where
  phase :: Text
  phase = case (nodeAt w 0).headState of
    Head.Open{} -> "open"
    Head.Closed{} -> "closed"
    Head.FanoutProgress{} -> "fanout"
    Head.Finalized{} -> "finalized"
    Head.Idle{} -> if busy w then "opening" else "setup"
  visible = visibleFunds w
  amount field places = sum [field chip | (_, chip, place) <- visible, place `elem` places]

personView :: Person -> Value
personView Person{..} =
  object
    ["id" .= personId, "name" .= name, "role" .= (if runsNode then "operator" else "user" :: Text), "node" .= ("operator-" <> show operator :: Text)]

signingView :: World -> Maybe Value
signingView w = do
  open <- openState $ nodeAt w 0
  let coordinated = mapMaybe (fmap (.coordinatedHeadState) . openState) w.nodes
      pending =
        viaNonEmpty maximum $
          [snapshotNumber | Broadcast _ Network.ReqSn{snapshotNumber} <- w.queue]
            <> mapMaybe (candidateNumber . (.seenSnapshot)) coordinated
      confirmed = (Snapshot.getSnapshot open.coordinatedHeadState.confirmedSnapshot).number
      number = fromMaybe (max 1 confirmed) pending
      status :: Text
      status
        | isJust pending = "collecting"
        | confirmed == 0 = "next"
        | otherwise = "confirmed"
      received party c = case c.seenSnapshot of
        Head.SeenSnapshot{cachedSnapshot, signatories}
          | (Cached.snapshot cachedSnapshot).number == number -> Map.member party signatories
        _ -> number == (Snapshot.getSnapshot c.confirmedSnapshot).number
      operators = filter (.runsNode) w.people
  pure $
    object
      [ "number" .= number
      , "status" .= status
      , "leader" .= (personId <$> find (\p -> isLeader open.parameters (partyFor p.operator) number) operators)
      , "signers" .= [p.personId | p <- operators, all (received $ partyFor p.operator) coordinated]
      ]

candidateNumber :: Head.SeenSnapshot SimpleTx -> Maybe Snapshot.SnapshotNumber
candidateNumber = \case
  Head.SeenSnapshot{cachedSnapshot} -> Just (Cached.snapshot cachedSnapshot).number
  Head.RequestedSnapshot{requested} -> Just requested
  _ -> Nothing

fundView :: World -> (SimpleTxOut, Chip, Text) -> Value
fundView w (output, Chip{..}, place) =
  object ["id" .= chipId, "owner" .= ownerId, "ada" .= ada, "tokens" .= tokens, "place" .= place, "output" .= output, "reservation" .= reservation]
 where
  reservation = viaNonEmpty head $ mapMaybe reserved w.queue
  reserved = \case
    WalletDeposit _ out | out == output -> Just ("Reserved for deposit; awaiting L1 confirmation" :: Text)
    Broadcast _ (Network.ReqTx tx) | output `Set.member` tx.txInputs -> Just "Reserved for payment; awaiting delivery to operators"
    Broadcast _ (Network.ReqDec tx) | output `Set.member` tx.txInputs -> Just "Reserved for withdrawal; awaiting delivery to operators"
    _ -> Nothing

visibleFunds :: World -> [(SimpleTxOut, Chip, Text)]
visibleFunds w =
  nubBy (\(_, a, _) (_, b, _) -> a.chipId == b.chipId) $
    tagged "l1" w.wallets
      <> tagged "deposit" (fold w.l1Deposits)
      <> tagged "closed" (foldMap closedFunds w.nodes)
      <> tagged "withdrawal" withdrawing
      <> tagged "payment" (local `Set.difference` confirmed)
      <> tagged "l2" (local `Set.intersection` confirmed)
      <> tagged "payment" confirmed
 where
  tagged place utxo = [(out, chip, place) | out <- toList utxo, Just chip <- [Map.lookup out w.chips]]
  local = foldMap localFunds w.nodes
  confirmed = foldMap confirmedFunds w.nodes
  withdrawing = foldMap (maybe mempty pending . openState) w.nodes
  pending o =
    let c = o.coordinatedHeadState
        s = Snapshot.getSnapshot c.confirmedSnapshot
     in foldMap txOutputs c.decommitTx
          <> if settlingDecommit c.version c.confirmedSnapshot then fold s.utxoToDecommit else mempty

destinations :: World -> Text -> [Value]
destinations w chipId
  | busy w || not (all (isJust . openState) w.nodes) = []
  | otherwise = case availableChip w chipId of
      Nothing -> []
      Just (_, chip, True) -> [target chip.ownerId "l2" "Put into the Head"]
      Just (out, chip, False) ->
        [target chip.ownerId "l1" "Take out to L1" | accepted chip $ Client.Decommit $ transaction w [out]]
          <> [target p.personId "l2" ("Pay " <> p.name) | p <- w.people, p.personId /= chip.ownerId, accepted chip $ Client.NewTx $ transaction w [out]]
 where
  target owner layer label = object ["owner" .= owner, "layer" .= (layer :: Text), "label" .= (label :: Text)]
  accepted chip command = case find ((== chip.ownerId) . personId) w.people of
    Nothing -> False
    Just p -> case fst $ evaluateInput w p.operator (ClientInput command) of
      Logic.Continue{effects} -> any (\case Logic.NetworkEffect{} -> True; _ -> False) effects
      _ -> False

guidance :: World -> Value
guidance w = case w.queue of
  Posted Chain.InitTx{} : _ -> prompt "Opening an empty Head" "The Init transaction names the agreed operators. Each node checks that this group and its configured parameters match." $ Just "Confirm opening on L1"
  WalletDeposit{} : _ -> prompt "A wallet has submitted a deposit" "The selected output is reserved for this deposit. It stays in the L1 wallet until confirmation and cannot be moved again." $ Just "Confirm deposit on L1"
  Broadcast i message : _ -> case message of
    Network.ReqTx{} -> prompt "Share the payment" "Deliver the transaction to every operator, including its sender. Each node validates it against its local L2 funds." $ Just "Send transaction to operators"
    Network.ReqDec{} -> prompt "Share the withdrawal" "Each operator checks the withdrawal. A snapshot must approve it before L1 can release any funds." $ Just "Deliver withdrawal to operators"
    Network.ReqSn{snapshotNumber} -> prompt ("Check snapshot " <> show snapshotNumber) (nodeName w i <> " is the leader for this snapshot. Deliver its proposal so each operator can check and sign it.") $ Just "Check the snapshot proposal"
    Network.AckSn{snapshotNumber} -> prompt ("Collect signatures for snapshot " <> show snapshotNumber) "This delivers one operator’s signature to every node. Confirmation requires a matching signature from every operator." $ Just $ "Deliver " <> operatorName i <> "’s signature"
  Posted Chain.IncrementTx{} : _ -> prompt "Approved; the deposit is still on L1" "The operators have submitted the approved increment. One transaction can claim this deposit; every node observes the result." $ Just "Confirm increment on L1"
  Posted Chain.DecrementTx{} : _ -> prompt "Approved; the withdrawal is still locked" "The operators have submitted the approved decrement. L1 confirmation releases the withdrawal outputs to the owner’s wallet." $ Just "Confirm withdrawal on L1"
  Posted Chain.CloseTx{} : _ -> prompt "Closing the Head" "One operator submits the latest confirmed snapshot to L1. Funds stay locked until the contestation period ends and fanout confirms." $ Just "Confirm close on L1"
  Posted Chain.FanoutTx{} : _ -> prompt "Returning funds to L1" "Confirm the fanout transaction to release the closed snapshot’s ADA and native tokens to their owners." $ Just "Confirm fanout on L1"
  RetryDelivery{} : _ -> prompt "A node is waiting" "The actual protocol deferred a message. Deliver its retry to recheck the condition; inspect the decision below." $ Just "Retry the waiting message"
  Posted{} : _ -> prompt "An L1 transaction is pending" "Inspect the pending transaction below." Nothing
  []
    | isJust (activationTime w) -> prompt "The deposit is locked, but not active yet" "Advance simulated chain time past the configured activation delay. The protocol then decides whether to propose a snapshot." $ Just "Let the deposit activate"
    | isJust (fanoutTime w) -> prompt "The Head is closed" "Funds stay locked during the contestation period. Advance simulated chain time past its deadline; operators can challenge an outdated snapshot during this period." $ Just "Wait contestation period"
    | canFanout w -> prompt "Ready to return funds" "The protocol has announced ReadyToFanout. Fanout returns the closed snapshot’s funds to L1." Nothing
    | Head.Finalized{} <- (nodeAt w 0).headState -> prompt "All funds are back on L1" "The Head is finalized. Rewind to inspect a step, or start over to try another flow." Nothing
    | all (isJust . openState) w.nodes -> prompt "The Head is open" "Drag funds into the Head to deposit, or back to L1 to withdraw. Use Send a payment below to move L2 funds between people." Nothing
    | otherwise -> prompt "Choose who runs this Head" "Operators agree on the group before opening. Other wallet users connect through a node and keep their keys." $ Just "Open the Head"
 where
  operatorName i = maybe "operator" (.name) $ find (\p -> p.runsNode && p.operator == i) w.people
  prompt title detail button = object ["title" .= (title :: Text), "detail" .= (detail :: Text), "button" .= (button :: Maybe Text)]

nodeView :: World -> Int -> Value
nodeView w i =
  object $
    [ "name" .= nodeName w i
    , "state" .= normalize w (toJSON n)
    , "components" .= Machine.inspect (Machine.start n)
    , "local" .= localFunds n
    , "confirmed" .= confirmedFunds n
    ]
      <> case n.headState of
        Head.Closed c -> snapshotView c.confirmedSnapshot c.version
        Head.FanoutProgress p -> snapshotView p.confirmedSnapshot p.version
        Head.Open o ->
          let c = o.coordinatedHeadState
           in [ "snapshot" .= (Snapshot.getSnapshot c.confirmedSnapshot).number
              , "version" .= c.version
              , "signers" .= case c.seenSnapshot of
                  Head.SeenSnapshot{signatories} -> [nodeName w j | j <- operatorIndices w, Map.member (partyFor j) signatories]
                  _ -> []
              , "candidate" .= candidateNumber c.seenSnapshot
              ]
        _ -> ["snapshot" .= Null, "version" .= Null, "signers" .= ([] :: [Text])]
 where
  n = nodeAt w i
  snapshotView confirmed version = ["snapshot" .= (Snapshot.getSnapshot confirmed).number, "version" .= version, "signers" .= ([] :: [Text])]

executionView :: World -> Execution -> Value
executionView w Execution{..} =
  object
    [ "operator" .= operatorIndex
    , "node" .= nodeName w operatorIndex
    , "input" .= normalize w (toJSON input)
    , "outcome" .= normalize w (toJSON outcome)
    , "after" .= normalize w (toJSON after)
    , "steps" .= map traceView decisions
    ]

traceView :: Trace.TraceStep -> Value
traceView = \case
  Trace.BooleanTraced label answer -> object ["label" .= label, "answer" .= answer]
  Trace.LedgerApplied valid -> object ["label" .= ("SimpleTx validation" :: Text), "answer" .= valid]
  Trace.PrimitiveCalled label -> labelOnly label
  Trace.Changed label -> labelOnly $ "State event: " <> label
  Trace.Emitted label -> labelOnly $ "Effect: " <> label
  Trace.Finished -> labelOnly "Finished"
  Trace.Suspended -> labelOnly "Waiting"
  Trace.Rejected -> labelOnly "Rejected"
 where
  labelOnly :: Text -> Value
  labelOnly label = object ["label" .= label]

normalize :: World -> Value -> Value
normalize w = go
 where
  identities = [(toJSON $ partyFor i, i) | i <- operatorIndices w] <> [(toJSON $ vkey $ partyFor i, i) | i <- operatorIndices w]
  signatureKeys = [(i, Key.fromText raw) | i <- operatorIndices w, String raw <- [toJSON $ vkey $ partyFor i]]
  go value
    | Just (_, i) <- find ((== value) . fst) identities = String $ "operator-" <> show i
    | otherwise = case value of
        Object fields -> Object $ KeyMap.fromList [field key val | (key, val) <- KeyMap.toList fields]
        Array values -> Array $ go <$> values
        _ -> value
  field key val
    | key `elem` ["accumulator", "appliedAccumulator"] = (key, String "content commitment")
    | key `elem` ["signature", "signed"] = (key, String "signature witness")
    | key == "multiSignature" = (key, case val of Array xs -> Array $ String "signature witness" <$ xs; _ -> go val)
    | key == "signatories"
    , Object signatures <- val =
        ( key
        , object
            [ Key.fromText ("operator-" <> show i) .= ("signature witness" :: Text)
            | (i, raw) <- signatureKeys
            , KeyMap.member raw signatures
            ]
        )
    | otherwise = (key, go val)
