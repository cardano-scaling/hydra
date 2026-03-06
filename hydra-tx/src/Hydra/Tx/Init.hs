module Hydra.Tx.Init where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (toList)

import GHC.IsList (toList)
import Hydra.Contract.Head qualified as Head
import Hydra.Contract.HeadState qualified as Head
import Hydra.Contract.HeadTokens qualified as HeadTokens
import Hydra.Contract.MintAction (MintAction (..))
import Hydra.Ledger.Cardano.Builder (addTxInsSpending, mintTokens, unsafeBuildTransaction)
import Hydra.Tx (hashUTxO)
import Hydra.Tx.ContestationPeriod (fromChain, toChain)
import Hydra.Tx.HeadId (HeadId, HeadSeed, mkHeadId, txInToHeadSeed)
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.OnChainId (OnChainId (..))
import Hydra.Tx.Party (partyFromChain, partyToChain)
import Hydra.Tx.Utils (assetNameToOnChainId, findFirst, hydraHeadV1AssetName, mkHydraHeadV1TxName, onChainIdToAssetName)
import PlutusLedgerApi.Common (FromData, toBuiltin)

-- * Construction

-- | Create the init transaction from some 'HeadParameters' and a single TxIn
-- which will be used as unique parameter for minting NFTs.
initTx ::
  NetworkId ->
  -- | Seed input.
  TxIn ->
  -- | Verification key hashes of all participants.
  [OnChainId] ->
  HeadParameters ->
  Tx
initTx networkId seedTxIn participants parameters =
  unsafeBuildTransaction $
    defaultTxBodyContent
      & addTxInsSpending [seedTxIn]
      & addTxOuts [mkHeadOutputOpen networkId seedTxIn parameters]
      & mintTokens (HeadTokens.mkHeadTokenScript seedTxIn) Mint (fromList $ (hydraHeadV1AssetName, 1) : participationTokens)
      & setTxMetadata (TxMetadataInEra $ mkHydraHeadV1TxName "InitTx")
 where
  participationTokens =
    [(onChainIdToAssetName oid, 1) | oid <- participants]

mkHeadOutput :: NetworkId -> PolicyId -> TxOutDatum ctx -> TxOut ctx
mkHeadOutput networkId tokenPolicyId datum =
  TxOut
    (mkScriptAddress networkId Head.validatorScript)
    (fromList [(AssetId tokenPolicyId hydraHeadV1AssetName, 1)])
    datum
    ReferenceScriptNone

mkHeadOutputOpen :: NetworkId -> TxIn -> HeadParameters -> TxOut CtxTx
mkHeadOutputOpen networkId seedTxIn HeadParameters{contestationPeriod, parties} =
  mkHeadOutput networkId tokenPolicyId headDatum
 where
  tokenPolicyId = HeadTokens.headPolicyId seedTxIn
  headDatum =
    mkTxOutDatumInline $
      Head.Open
        Head.OpenDatum
          { headSeed = toPlutusTxOutRef seedTxIn
          , headId = toPlutusCurrencySymbol tokenPolicyId
          , parties = map partyToChain parties
          , contestationPeriod = toChain contestationPeriod
          , version = 0
          , utxoHash = toBuiltin $ hashUTxO @Tx mempty
          }

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
  (pid, contestationPeriod, onChainParties, seedTxIn) <- case headState of
    Head.Open Head.OpenDatum{headSeed, headId, parties, contestationPeriod} -> do
      pid <- fromPlutusCurrencySymbol headId ?> NotAHeadPolicy
      pure (pid, fromChain contestationPeriod, parties, fromPlutusTxOutRef headSeed)
    _ -> Left NotAHeadDatum

  let stQuantity = selectAsset (txOutValue headOut) (AssetId pid hydraHeadV1AssetName)

  -- check that ST is present in the head output
  unless (stQuantity == 1) $
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
      , headParameters = HeadParameters{contestationPeriod, parties}
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
    , assetName /= hydraHeadV1AssetName
    ]
