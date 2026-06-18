module Hydra.Tx.Init where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (toList)

import Data.ByteString qualified as BS
import GHC.IsList (toList)
import Hydra.Contract.Head qualified as Head
import Hydra.Contract.HeadState qualified as Head
import Hydra.Contract.HeadTokens qualified as HeadTokens
import Hydra.Contract.MintAction (MintAction (..))
import Hydra.Ledger.Cardano.Builder (addTxInsSpending, mintTokens, unsafeBuildTransaction)
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.ContestationPeriod qualified as ContestationPeriod
import Hydra.Tx.DepositPeriod qualified as DepositPeriod
import Hydra.Tx.HeadId (HeadId, HeadSeed, mkHeadId, txInToHeadSeed)
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.OnChainId (OnChainId (..))
import Hydra.Tx.Party (partyFromChain, partyToChain)
import Hydra.Tx.Utils (assetNameToOnChainId, findFirst, hydraHeadV2AssetName, mkHydraHeadV2TxName, onChainIdToAssetName)
import PlutusLedgerApi.Common (FromData, toBuiltin)
import PlutusLedgerApi.V3 (POSIXTime (..), PubKeyHash (..))

-- * Construction

-- | Create the init transaction from some 'HeadParameters' and a single TxIn
-- which will be used as unique parameter for minting NFTs.
initTx ::
  NetworkId ->
  -- | Current protocol parameters, used to compute worst-case min-UTxO.
  PParams LedgerEra ->
  -- | Seed input.
  TxIn ->
  -- | Verification key hashes of all participants.
  [OnChainId] ->
  HeadParameters ->
  Tx
initTx networkId pparams seedTxIn participants parameters =
  unsafeBuildTransaction $
    defaultTxBodyContent
      & addTxInsSpending [seedTxIn]
      & addTxOuts [openHeadOutput]
      & mintTokens (HeadTokens.mkHeadTokenScript seedTxIn) Mint (fromList $ (hydraHeadV2AssetName, 1) : participationTokens)
      & setTxMetadata (TxMetadataInEra $ mkHydraHeadV2TxName "InitTx")
 where
  participationTokens =
    [(onChainIdToAssetName oid, 1) | oid <- participants]

  -- Set head output ADA to cover the worst-case min-UTxO: a ClosedDatum with
  -- N contesters (all parties contest, the maximum the datum can grow to).
  -- This ensures the wallet never needs to bump the head value at Close or
  -- Contest time, which would violate the strict equality check on-chain.
  openHeadOutput =
    modifyTxOutValue (worstCaseMinLovelace <>) $
      mkHeadOutput networkId tokenPolicyId participants openHeadDatum

  worstCaseMinLovelace =
    minUTxOValue pparams $
      mkHeadOutput networkId tokenPolicyId participants worstCaseClosedDatum

  worstCaseClosedDatum =
    mkTxOutDatumInline $
      Head.Closed
        Head.ClosedDatum
          { headId = toPlutusCurrencySymbol tokenPolicyId
          , parties = map partyToChain parties
          , contestationPeriod = ContestationPeriod.toChain contestationPeriod
          , depositPeriod = DepositPeriod.toChain depositPeriod
          , version = toInteger (maxBound @Word64)
          , snapshotNumber = toInteger (maxBound @Word64)
          , contesters = replicate (length participants) (PubKeyHash $ toBuiltin $ BS.replicate 28 0)
          , contestationDeadline = POSIXTime (toInteger (maxBound @Word64))
          , accumulatorCommitment =
              Accumulator.getAccumulatorCommitment $
                Accumulator.buildFromSnapshotUTxOs @Tx mempty Nothing Nothing
          , headAdaOverhead = toInteger (maxBound @Word64)
          }

  tokenPolicyId = HeadTokens.headPolicyId seedTxIn

  openHeadDatum =
    mkTxOutDatumInline $
      Head.Open
        Head.OpenDatum
          { headSeed = toPlutusTxOutRef seedTxIn
          , headId = toPlutusCurrencySymbol tokenPolicyId
          , parties = map partyToChain parties
          , contestationPeriod = ContestationPeriod.toChain contestationPeriod
          , depositPeriod = DepositPeriod.toChain depositPeriod
          , version = 0
          , accumulatorHash = toBuiltin $ Accumulator.getAccumulatorHash $ Accumulator.buildFromSnapshotUTxOs @Tx mempty Nothing Nothing
          , headAdaOverhead = let Coin n = selectLovelace worstCaseMinLovelace in n
          }

  HeadParameters{contestationPeriod, depositPeriod, parties} = parameters

mkHeadOutput :: NetworkId -> PolicyId -> [OnChainId] -> TxOutDatum ctx -> TxOut ctx
mkHeadOutput networkId tokenPolicyId participants datum =
  TxOut
    (mkScriptAddress networkId Head.validatorScript)
    (fromList $ st : pts)
    datum
    ReferenceScriptNone
 where
  st = (AssetId tokenPolicyId hydraHeadV2AssetName, 1)

  pts =
    [ (AssetId tokenPolicyId an, 1)
    | oid <- participants
    , let an = onChainIdToAssetName oid
    ]

-- * Observation

-- | Data which can be observed from an `initTx`.
data InitObservation = InitObservation
  { headId :: HeadId
  , headSeed :: HeadSeed
  , headParameters :: HeadParameters
  , participants :: [OnChainId]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data NotAnInitReason
  = NoHeadOutput
  | NotAHeadDatum
  | InvalidPartyInDatum
  | NoSTFound
  | NotAHeadPolicy
  | NoTokensMinted
  deriving stock (Show, Eq, Generic)

-- | Identify a init tx by checking the output value for holding tokens that are
-- valid head tokens (checked by seed + policy).
observeInitTx ::
  Tx ->
  Either NotAnInitReason InitObservation
observeInitTx tx = do
  (headOut, headState) <-
    findFirst matchHeadOutput (txOuts' tx) ?> NoHeadOutput

  -- check that we have a proper head
  (pid, contestationPeriod, depositPeriod, onChainParties, seedTxIn) <- case headState of
    Head.Open Head.OpenDatum{headSeed, headId, parties, contestationPeriod, depositPeriod} -> do
      pid <- fromPlutusCurrencySymbol headId ?> NotAHeadPolicy
      pure (pid, ContestationPeriod.fromChain contestationPeriod, DepositPeriod.fromChain depositPeriod, parties, fromPlutusTxOutRef headSeed)
    _ -> Left NotAHeadDatum

  -- Check minted value to distinguish from increment/decrement
  let mintedValue = txMintValueToValue . txMintValue . getTxBodyContent $ getTxBody tx
      stAssetId = AssetId pid hydraHeadV2AssetName
  unless (selectAsset mintedValue stAssetId == 1) $
    Left NoTokensMinted

  -- check that ST is present in the head output
  unless (selectAsset (txOutValue headOut) stAssetId == 1) $
    Left NoSTFound

  -- check that we are using the same seed and headId matches
  unless (pid == HeadTokens.headPolicyId seedTxIn) $
    Left NotAHeadPolicy

  parties <-
    maybe (Left InvalidPartyInDatum) Right $
      traverse partyFromChain onChainParties

  pure $
    InitObservation
      { headId = mkHeadId pid
      , headSeed = txInToHeadSeed seedTxIn
      , headParameters = HeadParameters{contestationPeriod, depositPeriod, parties}
      , participants = assetNameToOnChainId <$> mintedTokenNames pid
      }
 where
  matchHeadOutput :: FromData a => TxOut CtxTx -> Maybe (TxOut CtxTx, a)
  matchHeadOutput out = do
    guard $ isScriptTxOut Head.validatorScript out
    (out,) <$> (fromScriptData =<< txOutScriptData out)

  mintedTokenNames pid =
    [ assetName
    | (AssetId policyId assetName, q) <- toList $ txMintValueToValue $ txMintValue $ getTxBodyContent $ getTxBody tx
    , q == 1 -- NOTE: Only consider unique tokens
    , policyId == pid
    , assetName /= hydraHeadV2AssetName
    ]
