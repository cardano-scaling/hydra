module Hydra.Cardano.Api.Tx where

import Hydra.Cardano.Api.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.Allegra.Scripts (translateTimelock)
import Cardano.Ledger.Alonzo qualified as Ledger
import Cardano.Ledger.Alonzo.Scripts qualified as Ledger
import Cardano.Ledger.Alonzo.TxAuxData (translateAlonzoTxAuxData)
import Cardano.Ledger.Alonzo.TxWits qualified as Ledger
import Cardano.Ledger.Api (
  AlonzoPlutusPurpose (..),
  AsIx (..),
  Babbage,
  Conway,
  ConwayPlutusPurpose (..),
  EraTx (mkBasicTx),
  addrTxOutL,
  addrTxWitsL,
  auxDataHashTxBodyL,
  auxDataTxL,
  bodyTxL,
  bootAddrTxWitsL,
  collateralInputsTxBodyL,
  collateralReturnTxBodyL,
  dataTxOutL,
  datsTxWitsL,
  feeTxBodyL,
  getLanguageView,
  inputsTxBodyL,
  isValidTxL,
  mintTxBodyL,
  mkBasicTxBody,
  mkBasicTxOut,
  mkBasicTxWits,
  networkIdTxBodyL,
  outputsTxBodyL,
  rdmrsTxWitsL,
  referenceInputsTxBodyL,
  referenceScriptTxOutL,
  reqSignerHashesTxBodyL,
  scriptIntegrityHashTxBodyL,
  scriptTxWitsL,
  totalCollateralTxBodyL,
  valueTxOutL,
  vldtTxBodyL,
  withdrawalsTxBodyL,
  witsTxL,
 )
import Cardano.Ledger.Api qualified as Ledger
import Cardano.Ledger.Babbage qualified as Ledger
import Cardano.Ledger.Babbage.Tx (hashScriptIntegrity)
import Cardano.Ledger.Babbage.TxWits (upgradeTxDats)
import Cardano.Ledger.BaseTypes (maybeToStrictMaybe)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Scripts (PlutusScript (..))
import Cardano.Ledger.Conway.Scripts qualified as Conway
import Cardano.Ledger.Conway.TxBody qualified as Ledger
import Cardano.Ledger.Plutus.Data (upgradeData)
import Cardano.Ledger.Plutus.Language qualified as Ledger
import Control.Lens ((&), (.~), (^.))
import Data.Bifunctor (bimap)
import Data.Functor ((<&>))
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Hydra.Cardano.Api.TxIn (mkTxIn, toLedgerTxIn)

-- * Extras

-- | Sign transaction using the provided secret key
-- It only works for tx not containing scripts.
-- You can't sign a script utxo with this.
signTx ::
  IsShelleyBasedEra era =>
  SigningKey PaymentKey ->
  Tx era ->
  Tx era
signTx signingKey (Tx body wits) =
  makeSignedTransaction (witness : wits) body
 where
  witness = makeShelleyKeyWitness shelleyBasedEra body (WitnessPaymentKey signingKey)

-- | Create a transaction spending all given `UTxO`.
txSpendingUTxO :: UTxO -> Tx Era
txSpendingUTxO utxo =
  fromLedgerTx $
    mkBasicTx
      ( mkBasicTxBody
          & inputsTxBodyL .~ (toLedgerTxIn `Set.map` inputs)
      )
 where
  inputs = UTxO.inputSet utxo

-- | Get the UTxO that are produced by some transaction.
-- XXX: Defined here to avoid cyclic module dependency
utxoProducedByTx :: Tx Era -> UTxO
utxoProducedByTx tx =
  UTxO.fromPairs $
    zip [0 ..] (txOuts body)
      <&> bimap (mkTxIn tx) toCtxUTxOTxOut
 where
  TxBody body = getTxBody tx

-- | Get explicit fees allocated to a transaction.
txFee' :: Tx era -> Coin
txFee' (getTxBody -> TxBody body) =
  case txFee body of
    TxFeeExplicit _ y -> y

-- * Type Conversions

-- | Convert a cardano-api 'Tx' into a matching cardano-ledger 'Tx'.
toLedgerTx ::
  Tx era ->
  Ledger.Tx (ShelleyLedgerEra era)
toLedgerTx (ShelleyTx _era tx) = tx

-- | Convert a cardano-ledger's 'Tx' in the Babbage era into a cardano-api 'Tx'.
fromLedgerTx ::
  IsShelleyBasedEra era =>
  Ledger.Tx (ShelleyLedgerEra era) ->
  Tx era
fromLedgerTx =
  ShelleyTx shelleyBasedEra

-- | Compute the integrity hash of a transaction using a list of plutus languages.
recomputeIntegrityHash ::
  (Ledger.AlonzoEraPParams ppera, Ledger.AlonzoEraTxWits txera, Ledger.AlonzoEraTxBody txera, EraTx txera) =>
  Ledger.PParams ppera ->
  [Ledger.Language] ->
  Ledger.Tx txera ->
  Ledger.Tx txera
recomputeIntegrityHash pp languages tx = do
  tx & bodyTxL . scriptIntegrityHashTxBodyL .~ integrityHash
 where
  integrityHash =
    hashScriptIntegrity
      (Set.fromList $ getLanguageView pp <$> languages)
      (tx ^. witsTxL . rdmrsTxWitsL)
      (tx ^. witsTxL . datsTxWitsL)

-- | Explicit downgrade from Conway to Babbage era.
--
-- XXX: This will invalidate the script integrity hash as datums and redeemers
-- are serialized differently.
--
-- XXX: This is not a complete mapping and does silently drop things like
-- protocol updates, certificates and voting procedures.
convertConwayTx :: Ledger.Tx Conway -> Ledger.Tx Babbage
convertConwayTx tx =
  mkBasicTx (translateBody $ tx ^. bodyTxL)
    & witsTxL .~ translateWits (tx ^. witsTxL)
    & isValidTxL .~ tx ^. isValidTxL
    & auxDataTxL .~ (translateAlonzoTxAuxData <$> tx ^. auxDataTxL)
 where
  translateBody ::
    Ledger.ConwayTxBody Ledger.Conway ->
    Ledger.BabbageTxBody Ledger.Babbage
  translateBody body =
    mkBasicTxBody
      & inputsTxBodyL .~ body ^. inputsTxBodyL
      & outputsTxBodyL .~ (translateTxOut <$> body ^. outputsTxBodyL)
      & feeTxBodyL .~ body ^. feeTxBodyL
      & withdrawalsTxBodyL .~ body ^. withdrawalsTxBodyL
      & auxDataHashTxBodyL .~ body ^. auxDataHashTxBodyL
      -- NOTE: not considering 'updateTxBodyL' as upstream also does not upgrade it
      -- NOTE: not considering 'certsTxBodyL' as we are not interested in it
      & vldtTxBodyL .~ body ^. vldtTxBodyL
      & mintTxBodyL .~ body ^. mintTxBodyL
      & collateralInputsTxBodyL .~ body ^. collateralInputsTxBodyL
      & reqSignerHashesTxBodyL .~ body ^. reqSignerHashesTxBodyL
      & scriptIntegrityHashTxBodyL .~ body ^. scriptIntegrityHashTxBodyL
      & networkIdTxBodyL .~ body ^. networkIdTxBodyL
      & referenceInputsTxBodyL .~ body ^. referenceInputsTxBodyL
      & totalCollateralTxBodyL .~ body ^. totalCollateralTxBodyL
      & collateralReturnTxBodyL .~ (translateTxOut <$> body ^. collateralReturnTxBodyL)

  translateTxOut ::
    Ledger.BabbageTxOut Ledger.Conway ->
    Ledger.BabbageTxOut Ledger.Babbage
  translateTxOut out =
    mkBasicTxOut (out ^. addrTxOutL) (out ^. valueTxOutL)
      & dataTxOutL .~ (upgradeData <$> out ^. dataTxOutL)
      & referenceScriptTxOutL .~ (out ^. referenceScriptTxOutL >>= maybeToStrictMaybe . translateScript)

  translateWits ::
    Ledger.AlonzoTxWits Ledger.Conway ->
    Ledger.AlonzoTxWits Ledger.Babbage
  translateWits wits =
    mkBasicTxWits
      & addrTxWitsL .~ wits ^. addrTxWitsL
      & bootAddrTxWitsL .~ wits ^. bootAddrTxWitsL
      & scriptTxWitsL .~ Map.mapMaybe translateScript (wits ^. scriptTxWitsL)
      & datsTxWitsL .~ upgradeTxDats (wits ^. datsTxWitsL)
      & rdmrsTxWitsL .~ translateRdmrs (wits ^. rdmrsTxWitsL)

  translateScript ::
    Ledger.AlonzoScript Ledger.Conway ->
    Maybe (Ledger.AlonzoScript Ledger.Babbage)
  translateScript = \case
    Ledger.TimelockScript ts -> Just . Ledger.TimelockScript $ translateTimelock ts
    Ledger.PlutusScript ps -> case ps of
      ConwayPlutusV1 p1 -> Just . Ledger.PlutusScript $ BabbagePlutusV1 p1
      ConwayPlutusV2 p2 -> Just . Ledger.PlutusScript $ BabbagePlutusV2 p2
      ConwayPlutusV3{} -> Nothing

  translateRdmrs ::
    Ledger.Redeemers Ledger.Conway ->
    Ledger.Redeemers Ledger.Babbage
  translateRdmrs (Ledger.Redeemers redeemerMap) =
    Ledger.Redeemers
      . Map.fromList
      $ mapMaybe
        ( \(purpose, (dat, units)) -> do
            p' <- translatePlutusPurpose purpose
            pure (p', (upgradeData dat, units))
        )
      $ Map.toList redeemerMap

  translatePlutusPurpose ::
    Conway.ConwayPlutusPurpose Ledger.AsIx Ledger.Conway ->
    Maybe (Ledger.AlonzoPlutusPurpose Ledger.AsIx Ledger.Babbage)
  translatePlutusPurpose = \case
    ConwaySpending (AsIx ix) -> Just $ AlonzoSpending (AsIx ix)
    ConwayMinting (AsIx ix) -> Just $ AlonzoMinting (AsIx ix)
    ConwayCertifying (AsIx ix) -> Just $ AlonzoCertifying (AsIx ix)
    ConwayRewarding (AsIx ix) -> Just $ AlonzoRewarding (AsIx ix)
    ConwayVoting{} -> Nothing
    ConwayProposing{} -> Nothing
