module Hydra.Tx.Init where

import Hydra.Cardano.Api
import Hydra.Prelude

import Hydra.Contract.Head qualified as Head
import Hydra.Contract.HeadState qualified as Head
import Hydra.Contract.HeadTokens qualified as HeadTokens
import Hydra.Contract.Initial qualified as Initial
import Hydra.Contract.MintAction (MintAction (..))
import Hydra.Ledger.Cardano.Builder (addOutputs, addVkInputs, mintTokens, unsafeBuildTransaction)
import Hydra.Plutus (initialValidatorScript)
import Hydra.Tx.ContestationPeriod (toChain)
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.OnChainId (OnChainId (..))
import Hydra.Tx.Party (partyToChain)
import Hydra.Tx.Utils (hydraHeadV1AssetName, mkHydraHeadV1TxName, onChainIdToAssetName)

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
      & addVkInputs [seedTxIn]
      & addOutputs
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
    (mkScriptAddress @PlutusScriptV3 networkId headScript)
    (fromList [(AssetId tokenPolicyId hydraHeadV1AssetName, 1)])
    datum
    ReferenceScriptNone
 where
  headScript = fromPlutusScript Head.validatorScript

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
    mkScriptAddress @PlutusScriptV3 networkId initialScript
  initialScript =
    fromPlutusScript @PlutusScriptV3 initialValidatorScript
  initialDatum =
    mkTxOutDatumInline $ Initial.datum (toPlutusCurrencySymbol tokenPolicyId)
