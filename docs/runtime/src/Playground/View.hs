{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Playground.View (render) where

import Hydra.Prelude hiding (local, state)

import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List (nubBy)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Hydra.API.ClientInput qualified as Client
import Hydra.Chain qualified as Chain
import Hydra.HeadLogic.Input (Input (ClientInput))
import Hydra.HeadLogic.Outcome qualified as Logic
import Hydra.HeadLogic.Snapshot.Cached qualified as Cached
import Hydra.HeadLogic.Snapshot.Settlement (settlingDecommit)
import Hydra.HeadLogic.State qualified as Head
import Hydra.HeadLogic.State.Node qualified as Machine
import Hydra.HeadLogic.Trace qualified as Trace
import Hydra.Ledger.Simple
import Hydra.Network.Message qualified as Network
import Hydra.Tx.Party (Party (..))
import Hydra.Tx.Snapshot qualified as Snapshot
import Playground.Engine

render :: Bool -> World -> Value
render includeAudit w =
  object
    [ "status" .= ("ok" :: Text)
    , "phase" .= phase
    , "pending" .= if busy w then Just (object ["kind" .= ("delivery" :: Text)]) else Nothing
    , "people" .= map personView w.people
    , "funds" .= map (fundView w) visible
    , "snapshot" .= maybe 0 (Snapshot.number . Snapshot.getSnapshot . (.coordinatedHeadState.confirmedSnapshot)) (openState $ nodeAt w 0)
    , "messages" .= w.messages
    , "totals"
        .= object
          [ "wallets" .= (10 * Set.size w.wallets)
          , "deposits" .= (10 * sum (map Set.size $ Map.elems w.l1Deposits))
          , "backing" .= (10 * w.backing)
          , "spendable" .= (10 * length [() | (_, _, place) <- visible, place == "l2"])
          , "pending" .= (10 * length [() | (_, _, place) <- visible, place `elem` ["payment", "withdrawal"]])
          ]
    , "action" .= guidance w
    , "targets" .= object [Key.fromText chip.chipId .= destinations w chip.chipId | (_, chip, _) <- visible]
    , "nodes" .= map (nodeView w) (operatorIndices w)
    , "decisions" .= map (executionView w) (drop w.actionStart w.executions)
    , "audit" .= if includeAudit then map (executionView w) w.executions else []
    ]
 where
  phase :: Text
  phase
    | all (isJust . openState) w.nodes = "open"
    | busy w = "opening"
    | otherwise = "setup"
  visible = visibleFunds w

personView :: Person -> Value
personView Person{..} =
  object
    ["id" .= personId, "name" .= name, "role" .= (if runsNode then "operator" else "user" :: Text), "node" .= ("operator-" <> show operator :: Text)]

fundView :: World -> (SimpleTxOut, Chip, Text) -> Value
fundView w (output, Chip{..}, place) =
  object ["id" .= chipId, "owner" .= ownerId, "place" .= place, "output" .= output, "reservation" .= reservation]
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
  RetryDelivery{} : _ -> prompt "A node is waiting" "The actual protocol deferred a message. Deliver its retry to recheck the condition; inspect the decision below." $ Just "Retry the waiting message"
  Posted{} : _ -> prompt "An L1 transaction is pending" "Inspect the pending transaction below." Nothing
  []
    | isJust (activationTime w) -> prompt "The deposit is locked, but not active yet" "Advance simulated chain time past the configured activation delay. The protocol then decides whether to propose a snapshot." $ Just "Let the deposit activate"
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
      <> case openState n of
        Nothing -> ["snapshot" .= (0 :: Int), "version" .= (0 :: Int), "signers" .= ([] :: [Text])]
        Just o ->
          let c = o.coordinatedHeadState
           in [ "snapshot" .= (Snapshot.getSnapshot c.confirmedSnapshot).number
              , "version" .= c.version
              , "signers" .= case c.seenSnapshot of
                  Head.SeenSnapshot{signatories} -> [nodeName w j | j <- operatorIndices w, Map.member (partyFor j) signatories]
                  _ -> []
              , "candidate" .= case c.seenSnapshot of
                  Head.SeenSnapshot{cachedSnapshot} -> Just (Cached.snapshot cachedSnapshot).number
                  Head.RequestedSnapshot{requested} -> Just requested
                  _ -> Nothing
              ]
 where
  n = nodeAt w i

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
