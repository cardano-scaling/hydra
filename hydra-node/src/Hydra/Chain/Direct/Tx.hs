{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Smart constructors for creating Hydra protocol transactions to be used in
-- the 'Hydra.Chain.Direct' way of talking to the main-chain.
--
-- This module also encapsulates the transaction format used when talking to the
-- cardano-node, which is currently different from the 'Hydra.Ledger.Cardano',
-- thus we have not yet "reached" 'isomorphism'.
module Hydra.Chain.Direct.Tx (
  module Hydra.Chain.Direct.Tx,
) where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (toList)

import Cardano.Api.UTxO qualified as UTxO
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import GHC.IsList (IsList (..))
import Hydra.Contract.Commit qualified as Commit
import Hydra.Contract.Head qualified as Head
import Hydra.Contract.HeadState qualified as Head
import Hydra.Contract.HeadTokens qualified as HeadTokens
import Hydra.Data.ContestationPeriod qualified as OnChain
import Hydra.Data.Party qualified as OnChain
import Hydra.Plutus (commitValidatorScript, initialValidatorScript, depositValidatorScript)
import Hydra.Plutus.Extras (posixToUTCTime)
import Hydra.Plutus.Orphans ()
import Hydra.Tx (
  HeadId (..),
  HeadSeed (..),
  Party,
  SnapshotNumber,
  SnapshotVersion,
  fromChainSnapshotNumber,
  fromChainSnapshotVersion,
  headIdToCurrencySymbol,
  mkHeadId,
  partyFromChain,
 )
import Hydra.Tx.Close (OpenThreadOutput (..))
import Hydra.Tx.Contest (ClosedThreadOutput (..))
import Hydra.Tx.ContestationPeriod (ContestationPeriod, fromChain)
import Hydra.Tx.Deposit (DepositObservation (..), observeDepositTx)
import Hydra.Tx.OnChainId (OnChainId (..))
import Hydra.Tx.Recover (RecoverObservation (..), observeRecoverTx)
import Hydra.Tx.Utils (assetNameToOnChainId, findFirst, hydraHeadV1AssetName)
import PlutusLedgerApi.V3 (CurrencySymbol, fromBuiltin)
import PlutusLedgerApi.V3 qualified as Plutus
import Test.Hydra.Tx.Gen ()
import Test.QuickCheck (vectorOf)

-- | Needed on-chain data to create Head transactions.
type UTxOWithScript = (TxIn, TxOut CtxUTxO, HashableScriptData)

newtype UTxOHash = UTxOHash ByteString
  deriving stock (Eq, Show, Generic)

instance ToJSON UTxOHash where
  toJSON (UTxOHash bytes) =
    Aeson.String . decodeUtf8 $ Base16.encode bytes

instance FromJSON UTxOHash where
  parseJSON = Aeson.withText "UTxOHash" $ \cborText ->
    case Base16.decode $ encodeUtf8 cborText of
      Left e -> fail e
      Right bs -> pure $ UTxOHash bs

instance Arbitrary UTxOHash where
  arbitrary = UTxOHash . BS.pack <$> vectorOf 32 arbitrary

-- | Representation of the Head output after an Init transaction.
data InitialThreadOutput = InitialThreadOutput
  { initialThreadUTxO :: (TxIn, TxOut CtxUTxO)
  , initialContestationPeriod :: OnChain.ContestationPeriod
  , initialParties :: [OnChain.Party]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary InitialThreadOutput where
  arbitrary = genericArbitrary
  shrink = genericShrink

-- * Observe Hydra Head transactions

-- | Generalised type for arbitrary Head observations on-chain.
data HeadObservation
  = NoHeadTx
  | Init InitObservation
  | Abort AbortObservation
  | Commit CommitObservation
  | CollectCom CollectComObservation
  | Deposit DepositObservation
  | Recover RecoverObservation
  | Increment IncrementObservation
  | Decrement DecrementObservation
  | Close CloseObservation
  | Contest ContestObservation
  | Fanout FanoutObservation
  deriving stock (Eq, Show, Generic)

instance Arbitrary HeadObservation where
  arbitrary = genericArbitrary

-- | Observe any Hydra head transaction.
observeHeadTx :: NetworkId -> UTxO -> Tx -> HeadObservation
observeHeadTx networkId utxo tx =
  fromMaybe NoHeadTx $
    either (const Nothing) (Just . Init) (observeInitTx tx)
      <|> Abort <$> observeAbortTx utxo tx
      <|> Commit <$> observeCommitTx networkId utxo tx
      <|> CollectCom <$> observeCollectComTx utxo tx
      <|> Deposit <$> observeDepositTx networkId tx
      <|> Recover <$> observeRecoverTx networkId utxo tx
      <|> Increment <$> observeIncrementTx utxo tx
      <|> Decrement <$> observeDecrementTx utxo tx
      <|> Close <$> observeCloseTx utxo tx
      <|> Contest <$> observeContestTx utxo tx
      <|> Fanout <$> observeFanoutTx utxo tx

-- | Data which can be observed from an `initTx`.
data InitObservation = InitObservation
  { initialThreadUTxO :: (TxIn, TxOut CtxUTxO)
  , initials :: [(TxIn, TxOut CtxUTxO)]
  , headId :: HeadId
  , -- XXX: This is cardano-specific, while headId, parties and
    -- contestationPeriod are already generic here. Check which is more
    -- convenient and consistent!
    seedTxIn :: TxIn
  , contestationPeriod :: ContestationPeriod
  , parties :: [Party]
  , -- XXX: Improve naming
    participants :: [OnChainId]
  }
  deriving stock (Show, Eq, Generic)

instance Arbitrary InitObservation where
  arbitrary = genericArbitrary

data NotAnInitReason
  = NoHeadOutput
  | NotAHeadDatum
  | NoSTFound
  | NotAHeadPolicy
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary NotAnInitReason where
  arbitrary = genericArbitrary

-- | Identify a init tx by checking the output value for holding tokens that are
-- valid head tokens (checked by seed + policy).
observeInitTx ::
  Tx ->
  Either NotAnInitReason InitObservation
observeInitTx tx = do
  -- XXX: Lots of redundant information here
  (ix, headOut, headState) <-
    maybeLeft NoHeadOutput $
      findFirst matchHeadOutput indexedOutputs

  -- check that we have a proper head
  (pid, contestationPeriod, onChainParties, seedTxIn) <- case headState of
    (Head.Initial cp ps cid outRef) -> do
      pid <- fromPlutusCurrencySymbol cid ?> NotAHeadPolicy
      pure (pid, fromChain cp, ps, fromPlutusTxOutRef outRef)
    _ -> Left NotAHeadDatum

  let stQuantity = selectAsset (txOutValue headOut) (AssetId pid hydraHeadV1AssetName)

  -- check that ST is present in the head output
  unless (stQuantity == 1) $
    Left NoSTFound

  -- check that we are using the same seed and headId matches
  unless (pid == HeadTokens.headPolicyId seedTxIn) $
    Left NotAHeadPolicy

  pure $
    InitObservation
      { headId = mkHeadId pid
      , seedTxIn
      , initialThreadUTxO = (mkTxIn tx ix, toCtxUTxOTxOut headOut)
      , initials
      , contestationPeriod
      , parties = mapMaybe partyFromChain onChainParties
      , participants = assetNameToOnChainId <$> mintedTokenNames pid
      }
 where
  maybeLeft e = maybe (Left e) Right

  matchHeadOutput (ix, out) = do
    guard $ isScriptTxOut headScript out
    (ix,out,) <$> (fromScriptData =<< txOutScriptData out)

  headScript = fromPlutusScript @PlutusScriptV3 Head.validatorScript

  indexedOutputs = zip [0 ..] (txOuts' tx)

  initialOutputs = filter (isInitial . snd) indexedOutputs

  initials =
    map
      (bimap (mkTxIn tx) toCtxUTxOTxOut)
      initialOutputs

  isInitial = isScriptTxOut initialScript

  initialScript = fromPlutusScript @PlutusScriptV3 initialValidatorScript

  mintedTokenNames pid =
    [ assetName
    | (AssetId policyId assetName, q) <- txMintAssets tx
    , q == 1 -- NOTE: Only consider unique tokens
    , policyId == pid
    , assetName /= hydraHeadV1AssetName
    ]

-- | Full observation of a commit transaction.
data CommitObservation = CommitObservation
  { commitOutput :: (TxIn, TxOut CtxUTxO)
  , party :: Party
  -- ^ Hydra participant who committed the UTxO.
  , committed :: UTxO
  , headId :: HeadId
  }
  deriving stock (Eq, Show, Generic)

instance Arbitrary CommitObservation where
  arbitrary = genericArbitrary

-- | Identify a commit tx by:
--
-- - Check that its spending from the init validator,
-- - Find the outputs which pays to the commit validator,
-- - Using the datum of that output, deserialize the committed output,
-- - Reconstruct the committed UTxO from both values (tx input and output).
observeCommitTx ::
  NetworkId ->
  -- | A UTxO set to lookup tx inputs. Should at least contain the input
  -- spending from νInitial.
  UTxO ->
  Tx ->
  Maybe CommitObservation
observeCommitTx networkId utxo tx = do
  -- NOTE: Instead checking to spend from initial we could be looking at the
  -- seed:
  --
  --  - We must check that participation token in output satisfies
  --      policyId = hash(mu_head(seed))
  --
  --  - This allows us to assume (by induction) the output datum at the commit
  --    script is legit
  --
  --  - Further, we need to assert / assume that only one script is spent = onle
  --    one redeemer matches the InitialRedeemer, as we do not have information
  --    which of the inputs is spending from the initial script otherwise.
  --
  --  Right now we only have the headId in the datum, so we use that in place of
  --  the seed -> THIS CAN NOT BE TRUSTED.
  guard isSpendingFromInitial

  (commitIn, commitOut) <- findTxOutByAddress commitAddress tx
  dat <- txOutScriptData commitOut
  (onChainParty, onChainCommits, headId) :: Commit.DatumType <- fromScriptData dat
  party <- partyFromChain onChainParty

  -- NOTE: If we have the resolved inputs (utxo) then we could avoid putting
  -- the commit into the datum (+ changing the hashing strategy of
  -- collect/fanout)
  committed <- do
    committedUTxO <- traverse (Commit.deserializeCommit (networkIdToNetwork networkId)) onChainCommits
    pure . UTxO.fromPairs $ committedUTxO

  policyId <- fromPlutusCurrencySymbol headId
  pure
    CommitObservation
      { commitOutput = (commitIn, toUTxOContext commitOut)
      , party
      , committed
      , headId = mkHeadId policyId
      }
 where
  isSpendingFromInitial :: Bool
  isSpendingFromInitial =
    any (\o -> txOutAddress o == initialAddress) (resolveInputsUTxO utxo tx)

  initialAddress = mkScriptAddress @PlutusScriptV3 networkId initialScript

  initialScript = fromPlutusScript @PlutusScriptV3 initialValidatorScript

  commitAddress = mkScriptAddress networkId commitScript

  commitScript = fromPlutusScript @PlutusScriptV3 commitValidatorScript

data CollectComObservation = CollectComObservation
  { threadOutput :: OpenThreadOutput
  , headId :: HeadId
  , utxoHash :: UTxOHash
  }
  deriving stock (Show, Eq, Generic)

instance Arbitrary CollectComObservation where
  arbitrary = genericArbitrary

-- | Identify a collectCom tx by lookup up the input spending the Head output
-- and decoding its redeemer.
observeCollectComTx ::
  -- | A UTxO set to lookup tx inputs
  UTxO ->
  Tx ->
  Maybe CollectComObservation
observeCollectComTx utxo tx = do
  let inputUTxO = resolveInputsUTxO utxo tx
  (headInput, headOutput) <- findTxOutByScript @PlutusScriptV3 inputUTxO headScript
  redeemer <- findRedeemerSpending tx headInput
  oldHeadDatum <- txOutScriptData $ toTxContext headOutput
  datum <- fromScriptData oldHeadDatum
  headId <- findStateToken headOutput
  case (datum, redeemer) of
    (Head.Initial{parties, contestationPeriod}, Head.CollectCom) -> do
      (newHeadInput, newHeadOutput) <- findTxOutByScript @PlutusScriptV3 (utxoFromTx tx) headScript
      newHeadDatum <- txOutScriptData $ toTxContext newHeadOutput
      utxoHash <- UTxOHash <$> decodeUtxoHash newHeadDatum
      pure
        CollectComObservation
          { threadOutput =
              OpenThreadOutput
                { openThreadUTxO = (newHeadInput, newHeadOutput)
                , openParties = parties
                , openContestationPeriod = contestationPeriod
                }
          , headId
          , utxoHash
          }
    _ -> Nothing
 where
  headScript = fromPlutusScript Head.validatorScript
  decodeUtxoHash datum =
    case fromScriptData datum of
      Just (Head.Open Head.OpenDatum{utxoHash}) -> Just $ fromBuiltin utxoHash
      _ -> Nothing

data IncrementObservation = IncrementObservation
  { headId :: HeadId
  , newVersion :: SnapshotVersion
  , depositTxId :: TxId
  }
  deriving stock (Show, Eq, Generic)

instance Arbitrary IncrementObservation where
  arbitrary = genericArbitrary

observeIncrementTx ::
  UTxO ->
  Tx ->
  Maybe IncrementObservation
observeIncrementTx utxo tx = do
  let inputUTxO = resolveInputsUTxO utxo tx
  (headInput, headOutput) <- findTxOutByScript @PlutusScriptV3 inputUTxO headScript
  (TxIn depositTxId _, depositOutput) <- findTxOutByScript @PlutusScriptV3 utxo depositScript
  dat <- txOutScriptData $ toTxContext depositOutput
  _ <- fromScriptData dat :: Maybe (CurrencySymbol, Plutus.POSIXTime, [Commit.Commit])
  redeemer <- findRedeemerSpending tx headInput
  oldHeadDatum <- txOutScriptData $ toTxContext headOutput
  datum <- fromScriptData oldHeadDatum
  headId <- findStateToken headOutput
  case (datum, redeemer) of
    (Head.Open{}, Head.Increment Head.IncrementRedeemer{}) -> do
      (_, newHeadOutput) <- findTxOutByScript @PlutusScriptV3 (utxoFromTx tx) headScript
      newHeadDatum <- txOutScriptData $ toTxContext newHeadOutput
      case fromScriptData newHeadDatum of
        Just (Head.Open Head.OpenDatum{version}) ->
          pure
            IncrementObservation
              { headId
              , newVersion = fromChainSnapshotVersion version
              , depositTxId
              }
        _ -> Nothing
    _ -> Nothing
 where
  depositScript = fromPlutusScript depositValidatorScript
  headScript = fromPlutusScript Head.validatorScript

data DecrementObservation = DecrementObservation
  { headId :: HeadId
  , newVersion :: SnapshotVersion
  , distributedOutputs :: [TxOut CtxUTxO]
  }
  deriving stock (Show, Eq, Generic)

instance Arbitrary DecrementObservation where
  arbitrary = genericArbitrary

observeDecrementTx ::
  UTxO ->
  Tx ->
  Maybe DecrementObservation
observeDecrementTx utxo tx = do
  let inputUTxO = resolveInputsUTxO utxo tx
  (headInput, headOutput) <- findTxOutByScript @PlutusScriptV3 inputUTxO headScript
  redeemer <- findRedeemerSpending tx headInput
  oldHeadDatum <- txOutScriptData $ toTxContext headOutput
  datum <- fromScriptData oldHeadDatum
  headId <- findStateToken headOutput
  case (datum, redeemer) of
    (Head.Open{}, Head.Decrement Head.DecrementRedeemer{numberOfDecommitOutputs}) -> do
      (_, newHeadOutput) <- findTxOutByScript @PlutusScriptV3 (utxoFromTx tx) headScript
      newHeadDatum <- txOutScriptData $ toTxContext newHeadOutput
      case fromScriptData newHeadDatum of
        Just (Head.Open Head.OpenDatum{version}) ->
          pure
            DecrementObservation
              { headId
              , newVersion = fromChainSnapshotVersion version
              , distributedOutputs =
                  toUTxOContext <$> txOuts' tx
                    & drop 1 -- NOTE: Head output must be in first position
                    & take (fromIntegral numberOfDecommitOutputs)
              }
        _ -> Nothing
    _ -> Nothing
 where
  headScript = fromPlutusScript Head.validatorScript

data CloseObservation = CloseObservation
  { threadOutput :: ClosedThreadOutput
  , headId :: HeadId
  , snapshotNumber :: SnapshotNumber
  }
  deriving stock (Show, Eq, Generic)

instance Arbitrary CloseObservation where
  arbitrary = genericArbitrary

-- | Identify a close tx by lookup up the input spending the Head output and
-- decoding its redeemer.
observeCloseTx ::
  -- | A UTxO set to lookup tx inputs
  UTxO ->
  Tx ->
  Maybe CloseObservation
observeCloseTx utxo tx = do
  let inputUTxO = resolveInputsUTxO utxo tx
  (headInput, headOutput) <- findTxOutByScript @PlutusScriptV3 inputUTxO headScript
  redeemer <- findRedeemerSpending tx headInput
  oldHeadDatum <- txOutScriptData $ toTxContext headOutput
  datum <- fromScriptData oldHeadDatum
  headId <- findStateToken headOutput
  case (datum, redeemer) of
    (Head.Open Head.OpenDatum{parties}, Head.Close{}) -> do
      (newHeadInput, newHeadOutput) <- findTxOutByScript @PlutusScriptV3 (utxoFromTx tx) headScript
      newHeadDatum <- txOutScriptData $ toTxContext newHeadOutput
      (closeContestationDeadline, onChainSnapshotNumber) <- case fromScriptData newHeadDatum of
        Just (Head.Closed Head.ClosedDatum{contestationDeadline, snapshotNumber}) ->
          pure (contestationDeadline, snapshotNumber)
        _ -> Nothing
      pure
        CloseObservation
          { threadOutput =
              ClosedThreadOutput
                { closedThreadUTxO = (newHeadInput, newHeadOutput)
                , closedParties = parties
                , closedContestationDeadline = closeContestationDeadline
                , closedContesters = []
                }
          , headId
          , snapshotNumber = fromChainSnapshotNumber onChainSnapshotNumber
          }
    _ -> Nothing
 where
  headScript = fromPlutusScript Head.validatorScript

data ContestObservation = ContestObservation
  { contestedThreadOutput :: (TxIn, TxOut CtxUTxO)
  , headId :: HeadId
  , snapshotNumber :: SnapshotNumber
  , contestationDeadline :: UTCTime
  , contesters :: [Plutus.PubKeyHash]
  }
  deriving stock (Show, Eq, Generic)

instance Arbitrary ContestObservation where
  arbitrary = genericArbitrary

-- | Identify a close tx by lookup up the input spending the Head output and
-- decoding its redeemer.
observeContestTx ::
  -- | A UTxO set to lookup tx inputs
  UTxO ->
  Tx ->
  Maybe ContestObservation
observeContestTx utxo tx = do
  let inputUTxO = resolveInputsUTxO utxo tx
  (headInput, headOutput) <- findTxOutByScript @PlutusScriptV3 inputUTxO headScript
  redeemer <- findRedeemerSpending tx headInput
  oldHeadDatum <- txOutScriptData $ toTxContext headOutput
  datum <- fromScriptData oldHeadDatum
  headId <- findStateToken headOutput
  case (datum, redeemer) of
    (Head.Closed Head.ClosedDatum{}, Head.Contest{}) -> do
      (newHeadInput, newHeadOutput) <- findTxOutByScript @PlutusScriptV3 (utxoFromTx tx) headScript
      newHeadDatum <- txOutScriptData $ toTxContext newHeadOutput
      let (onChainSnapshotNumber, contestationDeadline, contesters) = decodeDatum newHeadDatum
      pure
        ContestObservation
          { contestedThreadOutput = (newHeadInput, newHeadOutput)
          , headId
          , snapshotNumber = fromChainSnapshotNumber onChainSnapshotNumber
          , contestationDeadline = posixToUTCTime contestationDeadline
          , contesters
          }
    _ -> Nothing
 where
  headScript = fromPlutusScript Head.validatorScript

  decodeDatum headDatum =
    case fromScriptData headDatum of
      Just (Head.Closed Head.ClosedDatum{snapshotNumber, contestationDeadline, contesters}) ->
        (snapshotNumber, contestationDeadline, contesters)
      _ -> error "wrong state in output datum"

newtype FanoutObservation = FanoutObservation {headId :: HeadId} deriving stock (Eq, Show, Generic)

instance Arbitrary FanoutObservation where
  arbitrary = genericArbitrary

-- | Identify a fanout tx by lookup up the input spending the Head output and
-- decoding its redeemer.
observeFanoutTx ::
  -- | A UTxO set to lookup tx inputs
  UTxO ->
  Tx ->
  Maybe FanoutObservation
observeFanoutTx utxo tx = do
  let inputUTxO = resolveInputsUTxO utxo tx
  (headInput, headOutput) <- findTxOutByScript @PlutusScriptV3 inputUTxO headScript
  headId <- findStateToken headOutput
  findRedeemerSpending tx headInput
    >>= \case
      Head.Fanout{} -> pure FanoutObservation{headId}
      _ -> Nothing
 where
  headScript = fromPlutusScript Head.validatorScript

newtype AbortObservation = AbortObservation {headId :: HeadId} deriving stock (Eq, Show, Generic)

instance Arbitrary AbortObservation where
  arbitrary = genericArbitrary

-- | Identify an abort tx by looking up the input spending the Head output and
-- decoding its redeemer.
observeAbortTx ::
  -- | A UTxO set to lookup tx inputs
  UTxO ->
  Tx ->
  Maybe AbortObservation
observeAbortTx utxo tx = do
  let inputUTxO = resolveInputsUTxO utxo tx
  (headInput, headOutput) <- findTxOutByScript @PlutusScriptV3 inputUTxO headScript
  headId <- findStateToken headOutput
  findRedeemerSpending tx headInput >>= \case
    Head.Abort -> pure $ AbortObservation headId
    _ -> Nothing
 where
  headScript = fromPlutusScript Head.validatorScript

-- * Cardano specific identifiers

currencySymbolToHeadId :: MonadFail m => CurrencySymbol -> m HeadId
currencySymbolToHeadId = fmap mkHeadId . fromPlutusCurrencySymbol

headIdToPolicyId :: MonadFail m => HeadId -> m PolicyId
headIdToPolicyId = fromPlutusCurrencySymbol . headIdToCurrencySymbol

headSeedToTxIn :: MonadFail m => HeadSeed -> m TxIn
headSeedToTxIn (UnsafeHeadSeed bytes) =
  case Aeson.decodeStrict bytes of
    Nothing -> fail $ "Failed to decode HeadSeed " <> show bytes
    Just txIn -> pure txIn

txInToHeadSeed :: TxIn -> HeadSeed
txInToHeadSeed txin = UnsafeHeadSeed $ toStrict $ Aeson.encode txin

-- * Helpers

findHeadAssetId :: TxOut ctx -> Maybe (PolicyId, AssetName)
findHeadAssetId txOut =
  flip findFirst (toList $ txOutValue txOut) $ \case
    (AssetId pid aname, q)
      | aname == hydraHeadV1AssetName && q == 1 ->
          Just (pid, aname)
    _ ->
      Nothing

-- | Find (if it exists) the head identifier contained in given `TxOut`.
findStateToken :: TxOut ctx -> Maybe HeadId
findStateToken =
  fmap (mkHeadId . fst) . findHeadAssetId
