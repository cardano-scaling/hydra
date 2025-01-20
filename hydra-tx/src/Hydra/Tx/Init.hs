module Hydra.Tx.Init where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (toList)

import GHC.IsList (toList)
import Hydra.Contract.Head qualified as Head
import Hydra.Contract.HeadState qualified as Head
import Hydra.Contract.HeadTokens qualified as HeadTokens
import Hydra.Contract.Initial qualified as Initial
import Hydra.Contract.MintAction (MintAction (..))
import Hydra.Ledger.Cardano.Builder (addTxInsSpending, mintTokens, unsafeBuildTransaction)
import Hydra.Plutus (initialValidatorScript)
import Hydra.Tx.ContestationPeriod (ContestationPeriod, fromChain, toChain)
import Hydra.Tx.HeadId (HeadId, mkHeadId)
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.OnChainId (OnChainId (..))
import Hydra.Tx.Party (Party, partyFromChain, partyToChain)
import Hydra.Tx.Utils (assetNameToOnChainId, findFirst, hydraHeadV1AssetName, mkHydraHeadV1TxName, onChainIdToAssetName)

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
      & addTxOuts
        ( mkHeadOutputInitial networkId seedTxIn parameters
            : map (mkInitialOutput networkId seedTxIn) participants
        )
      & mintTokens (HeadTokens.mkHeadTokenScript seedTxIn) Mint ((hydraHeadV1AssetName, 1) : participationTokens)
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

mkHeadOutputInitial :: NetworkId -> TxIn -> HeadParameters -> TxOut CtxTx
mkHeadOutputInitial networkId seedTxIn HeadParameters{contestationPeriod, parties} =
  mkHeadOutput networkId tokenPolicyId headDatum
 where
  tokenPolicyId = HeadTokens.headPolicyId seedTxIn
  headDatum =
    mkTxOutDatumInline $
      Head.Initial
        { contestationPeriod = toChain contestationPeriod
        , parties = map partyToChain parties
        , headId = toPlutusCurrencySymbol tokenPolicyId
        , seed = toPlutusTxOutRef seedTxIn
        }

mkInitialOutput :: NetworkId -> TxIn -> OnChainId -> TxOut CtxTx
mkInitialOutput networkId seedTxIn participant =
  TxOut initialAddress initialValue initialDatum ReferenceScriptNone
 where
  tokenPolicyId = HeadTokens.headPolicyId seedTxIn
  initialValue =
    fromList [(AssetId tokenPolicyId (onChainIdToAssetName participant), 1)]
  initialAddress =
    mkScriptAddress networkId initialValidatorScript
  initialDatum =
    mkTxOutDatumInline $ Initial.datum (toPlutusCurrencySymbol tokenPolicyId)

-- * Observation

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

data NotAnInitReason
  = NoHeadOutput
  | NotAHeadDatum
  | NoSTFound
  | NotAHeadPolicy
  deriving stock (Show, Eq, Generic)

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
    guard $ isScriptTxOut Head.validatorScript out
    (ix,out,) <$> (fromScriptData =<< txOutScriptData out)

  indexedOutputs = zip [0 ..] (txOuts' tx)

  initialOutputs = filter (isInitial . snd) indexedOutputs

  initials =
    map
      (bimap (mkTxIn tx) toCtxUTxOTxOut)
      initialOutputs

  isInitial = isScriptTxOut initialValidatorScript

  mintedTokenNames pid =
    [ assetName
    | (AssetId policyId assetName, q) <- toList $ txMintValueToValue $ txMintValue $ getTxBodyContent $ getTxBody tx
    , q == 1 -- NOTE: Only consider unique tokens
    , policyId == pid
    , assetName /= hydraHeadV1AssetName
    ]
