{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Orphans ToJSON/FromJSON instances on ledger types used by
-- Hydra.Ledger.Cardano to have JSON representations for various types.
--
-- XXX: The ledger team notified that we should be using lenses going forward.
module Hydra.Ledger.Cardano.Json where

import Hydra.Cardano.Api
import Hydra.Prelude

import Cardano.Ledger.Api (Babbage, Conway)
import Cardano.Ledger.Api.Era (eraProtVerLow)
import Cardano.Ledger.Babbage.PParams (BabbagePParams (..))
import Cardano.Ledger.Babbage.PParams qualified as Ledger
import Cardano.Ledger.Conway.PParams (ConwayPParams (..), THKD)
import Cardano.Ledger.Conway.PParams qualified as Ledger
import Cardano.Ledger.HKD (HKD)
import Cardano.Ledger.Shelley.API qualified as Ledger
import Data.Aeson (
  (.!=),
  (.:),
  (.:?),
 )
import Data.Aeson qualified as Aeson

-- XXX: Maybe use babbagePParamsHKDPairs?
instance FromJSON (Ledger.BabbagePParams Identity era) where
  parseJSON =
    Aeson.withObject "PParams" $ \obj ->
      BabbagePParams
        <$> obj
          .: "minFeeA"
        <*> obj
          .: "minFeeB"
        <*> obj
          .: "maxBlockBodySize"
        <*> obj
          .: "maxTxSize"
        <*> obj
          .: "maxBlockHeaderSize"
        <*> obj
          .: "keyDeposit"
        <*> obj
          .: "poolDeposit"
        <*> obj
          .: "eMax"
        <*> obj
          .: "nOpt"
        <*> obj
          .: "a0"
        <*> obj
          .: "rho"
        <*> obj
          .: "tau"
        -- NOTE: 'protocolVersion' here is set to optional until the upstream
        -- bug fix is released. Relevant PR https://github.com/IntersectMBO/cardano-ledger/pull/3953
        <*> (obj .:? "protocolVersion" .!= Ledger.ProtVer (eraProtVerLow @Babbage) 0)
        <*> obj
          .: "minPoolCost"
          .!= mempty
        <*> obj
          .: "coinsPerUTxOByte"
        <*> obj
          .: "costmdls"
        <*> obj
          .: "prices"
        <*> obj
          .: "maxTxExUnits"
        <*> obj
          .: "maxBlockExUnits"
        <*> obj
          .: "maxValSize"
        <*> obj
          .: "collateralPercentage"
        <*> obj
          .: "maxCollateralInputs"

deriving newtype instance FromJSON (HKD f a) => FromJSON (THKD t f a)
deriving newtype instance Semigroup (HKD f a) => Semigroup (THKD t f a)
deriving newtype instance Monoid (HKD f a) => Monoid (THKD t f a)

instance FromJSON (Ledger.ConwayPParams Identity era) where
  parseJSON =
    Aeson.withObject "PParams" $ \obj ->
      ConwayPParams
        <$> obj
          .: "minFeeA"
        <*> obj
          .: "minFeeB"
        <*> obj
          .: "maxBlockBodySize"
        <*> obj
          .: "maxTxSize"
        <*> obj
          .: "maxBlockHeaderSize"
        <*> obj
          .: "keyDeposit"
        <*> obj
          .: "poolDeposit"
        <*> obj
          .: "eMax"
        <*> obj
          .: "nOpt"
        <*> obj
          .: "a0"
        <*> obj
          .: "rho"
        <*> obj
          .: "tau"
        -- NOTE: 'protocolVersion' here is set to optional until the upstream
        -- bug fix is released. Relevant PR https://github.com/IntersectMBO/cardano-ledger/pull/3953
        <*> (obj .:? "protocolVersion" .!= Ledger.ProtVer (eraProtVerLow @Conway) 0)
        <*> obj
          .: "minPoolCost"
          .!= mempty
        <*> obj
          .: "coinsPerUTxOByte"
        <*> obj
          .: "costmdls"
        <*> obj
          .: "prices"
        <*> obj
          .: "maxTxExUnits"
        <*> obj
          .: "maxBlockExUnits"
        <*> obj
          .: "maxValSize"
        <*> obj
          .: "collateralPercentage"
        <*> obj
          .: "maxCollateralInputs"
        <*> obj
          .: "poolVotingThresholds"
        <*> obj
          .: "dRepVotingThresholds"
        <*> obj
          .: "comitteeMinSize"
        <*> obj
          .: "comitteeMaxTermLength"
        <*> obj
          .: "govActionLifetime"
        <*> obj
          .: "govActionDeposit"
          .!= mempty
        <*> obj
          .: "dRepDeposit"
        <*> obj
          .: "dRepActivity"
