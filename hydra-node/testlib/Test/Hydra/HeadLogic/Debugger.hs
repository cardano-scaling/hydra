{-# LANGUAGE RankNTypes #-}

-- | Development-only primitive recording/replay and instruction stepping. The
-- production interpreter has no fault switch or recorded-reply interface.
module Test.Hydra.HeadLogic.Debugger where

import Data.Aeson (Result (..), Value, fromJSON, object, (.=))
import Data.ByteString qualified as BS
import Hydra.HeadLogic.Interpreter (
  InterpreterConfig (snapshotSigner, transactionApplication),
  SnapshotSigner (signOne, signingIdentity),
  available,
 )
import Hydra.HeadLogic.Program (
  Instruction (..),
  Program (Program),
  decide,
  foldProgram,
  instruction,
 )
import Hydra.HeadLogic.Trace (changeLabel, effectLabel)
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Prelude hiding (keys, state)
import Hydra.Tx.Crypto (HydraKey, VerificationKey, Verified (..), verifyMultiSignatureBytes)

-- | Exact query identity and complete public arguments precede every reply.
data QueryRecord = QueryRecord
  { primitive :: Text
  , arguments :: Value
  , reply :: Value
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data VerificationReply = Valid | InvalidKeys [VerificationKey HydraKey] | InvalidCount
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ReplyMode = RecordReplies | RecordedReplies [QueryRecord]

-- | Semantic categories for presentation; labels are not parsed for routing.
data StepKind = InputStep | DecisionStep | LedgerStep | CryptoStep | ChangeStep | EffectStep | TerminalStep
  deriving stock (Eq, Show)

-- | Retained payloads let a debugger inspect each staged change, even when the
-- final rejection discards it. No signing key enters this structure.
data Step = Step
  { kind :: StepKind
  , description :: Text
  , decisionName :: Maybe Text
  , payload :: Value
  }
  deriving stock (Eq, Show, Generic)

data Prepared ledger signing = Prepared
  { lowered :: Program SimpleTx ledger signing ()
  , queries :: [QueryRecord]
  , steps :: [Step]
  }

-- | Reify a program one instruction at a time. Replay checks query name AND
-- arguments before decoding the typed reply. Extra or missing replies fail.
-- The lowered program still uses the shared completion/wait/reject semantics.
prepare :: forall ledger signing. ReplyMode -> InterpreterConfig SimpleTx ledger signing -> Program SimpleTx ledger signing () -> Either Text (Prepared ledger signing)
prepare mode config program = consume (cursor program) mode
 where
  consume :: Cursor SimpleTx ledger signing () -> ReplyMode -> Either Text (Prepared ledger signing)
  consume = \case
    Done () -> done ()
    Await op next -> step op (consume . next)

  done :: () -> ReplyMode -> Either Text (Prepared ledger signing)
  done () remaining = case remaining of
    RecordedReplies (_ : _) -> Left "unused recorded primitive replies"
    _ -> Right $ Prepared (pure ()) [] [Step TerminalStep "finish: commit changes and release effects" Nothing (toJSON ())]

  step :: forall a. Instruction SimpleTx ledger signing a -> (a -> ReplyMode -> Either Text (Prepared ledger signing)) -> ReplyMode -> Either Text (Prepared ledger signing)
  step op next remaining = case op of
    ApplyTransaction slot utxo tx -> query LedgerStep "apply transaction" (object ["slot" .= slot, "utxo" .= utxo, "tx" .= tx]) (available (transactionApplication config) slot utxo tx) id
    SignSnapshot snapshot -> query CryptoStep "sign snapshot" (object ["signer" .= signingIdentity (available $ snapshotSigner config), "snapshot" .= snapshot]) (signOne (available $ snapshotSigner config) snapshot) id
    VerifySnapshot keys signatures bytes ->
      query
        CryptoStep
        "verify multisignature"
        (object ["keys" .= keys, "signatures" .= signatures, "bytes" .= BS.unpack bytes])
        (encodeVerified $ verifyMultiSignatureBytes keys signatures bytes)
        decodeVerified
    Suspend reason -> terminal "wait: retain prefix, discard effects" (toJSON reason)
    Reject err -> terminal "reject: discard staged changes and effects" (toJSON err)
    Change event -> continue () (Step ChangeStep ("stage: " <> changeLabel event) Nothing (toJSON event))
    RecordAppliedTransaction hid tx before after -> continue () (Step ChangeStep "stage: validated transaction" Nothing (object ["headId" .= hid, "tx" .= tx, "before" .= before, "after" .= after]))
    Emit effect -> continue () (Step EffectStep ("buffer: " <> effectLabel effect) Nothing (toJSON effect))
    Decide name answer -> do
      rest <- next answer remaining
      pure rest{lowered = instruction op >> lowered rest, steps = Step DecisionStep (name <> ": " <> show answer) (Just name) (toJSON answer) : steps rest}
   where
    query :: (ToJSON b, FromJSON b) => StepKind -> Text -> Value -> b -> (b -> a) -> Either Text (Prepared ledger signing)
    query kind name args actual decode = do
      (answer, record, remaining') <- case remaining of
        RecordReplies -> Right (actual, QueryRecord name args (toJSON actual), RecordReplies)
        RecordedReplies [] -> Left $ "missing primitive reply: " <> name
        RecordedReplies (record : records)
          | primitive record /= name -> Left $ "primitive identity mismatch: expected " <> name
          | arguments record /= args -> Left $ "primitive arguments mismatch: " <> name
          | otherwise -> case fromJSON (reply record) of
              Error err -> Left $ "invalid typed reply for " <> name <> ": " <> toText err
              Success answer -> Right (answer, record, RecordedReplies records)
      rest <- next (decode answer) remaining'
      pure rest{queries = record : queries rest, steps = Step kind ("primitive: " <> name) Nothing (toJSON record) : steps rest}

    continue answer view = do
      rest <- next answer remaining
      pure rest{lowered = instruction op >> lowered rest, steps = view : steps rest}

    terminal :: Text -> Value -> Either Text (Prepared ledger signing)
    terminal name value = case remaining of
      RecordedReplies (_ : _) -> Left "unused recorded primitive replies at termination"
      _ -> Right $ Prepared (void $ instruction op) [] [Step TerminalStep name Nothing value]

encodeVerified :: Verified -> VerificationReply
encodeVerified = \case
  Verified -> Valid
  FailedKeys keys -> InvalidKeys keys
  KeyNumberMismatch -> InvalidCount

decodeVerified :: VerificationReply -> Verified
decodeVerified = \case
  Valid -> Verified
  InvalidKeys keys -> FailedKeys keys
  InvalidCount -> KeyNumberMismatch

-- | Test-only transformation used by the team demonstration. The independent
-- invariant still calls the real verifier on confirmed snapshots.
bypassVerification :: Program tx ledger signing a -> Program tx ledger signing a
bypassVerification = foldProgram $ \case
  VerifySnapshot{} -> decide "TEST ONLY: bypass multisignature verification" True >> pure Verified
  op -> instruction op

-- | Inspect a prepared instruction trace. Primitives have already been consumed;
-- use Cursor when a caller needs to supply replies while execution is suspended.
stepOnce :: [Step] -> Maybe (Step, [Step])
stepOnce = uncons

breakAt :: Text -> [Step] -> ([Step], [Step])
breakAt name = break ((== Just name) . decisionName)

-- | A live instruction cursor retains the typed continuation. Supplying a reply
-- advances exactly one instruction without running any later primitive.
data Cursor tx ledger signing a where
  Done :: a -> Cursor tx ledger signing a
  Await :: Instruction tx ledger signing x -> (x -> Cursor tx ledger signing a) -> Cursor tx ledger signing a

cursor :: Program tx ledger signing a -> Cursor tx ledger signing a
cursor (Program program) = program Done Await
