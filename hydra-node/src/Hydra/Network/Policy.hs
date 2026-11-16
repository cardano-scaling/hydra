module Hydra.Network.Policy where

import Hydra.Prelude

import Cardano.Binary (serialize')
import Cardano.Crypto.Hash (Blake2b_256, hashToStringAsHex, hashWithSerialiser)
import Cardano.Crypto.Util (SignableRepresentation (..))
import Codec.CBOR.Encoding (Encoding)
import Control.Concurrent.Class.MonadSTM (check, writeTVar)
import Control.Tracer (Tracer, traceWith)
import Data.Map.Strict qualified as Map
import Data.Secret (Secret)
import Data.Set qualified as Set
import Hydra.Network.Authenticate (Signed (..))
import Hydra.Tx (Party (..), deriveParty)
import Hydra.Tx.Crypto (HydraKey, SigningKey, sign, verify)
import Hydra.UserValidationRules (MintingOnL2Rule (..), PolicyRule (..), UserValidationRules (..))

newtype PolicyFingerprint = PolicyFingerprint Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, ToJSON, ToCBOR, FromCBOR)

data PolicyAnnouncement = PolicyAnnouncement
  { session :: Text
  , fingerprint :: PolicyFingerprint
  , acknowledgements :: Map Party Text
  }
  deriving stock (Eq, Show, Generic)

instance ToCBOR PolicyAnnouncement where
  toCBOR = genericToCBOR

instance FromCBOR PolicyAnnouncement where
  fromCBOR = genericFromCBOR

instance SignableRepresentation PolicyAnnouncement where
  getSignableRepresentation = serialize'

data PolicyStore m = PolicyStore
  { publish :: Signed PolicyAnnouncement -> m ()
  , load :: m [Signed PolicyAnnouncement]
  }

data PolicyLog
  = PolicyAgreementReached PolicyFingerprint
  | PolicyAgreementPending
      { ourPolicy :: PolicyFingerprint
      , missingParties :: [Party]
      , mismatchedPolicies :: [(Party, PolicyFingerprint)]
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

policyFingerprint :: UserValidationRules -> PolicyFingerprint
policyFingerprint =
  PolicyFingerprint . toText . hashToStringAsHex . hashWithSerialiser @Blake2b_256 encode
 where
  -- Fixed field order and sorted sets make this independent of YAML formatting.
  -- Change the domain version if the encoding or interpretation changes.
  encode :: UserValidationRules -> Encoding
  encode UserValidationRules{policyRule, l2MintingRule, maxUTxOCount, maxOutputValueAssets, maxDatumSize, allowedScriptAddresses, allowedDeposits} =
    toCBOR ("hydra-policy-v2" :: Text)
      <> toCBOR allowedPolicies
      <> toCBOR (l2MintingRule == ForbidMinting)
      <> toCBOR maxUTxOCount
      <> toCBOR maxOutputValueAssets
      <> toCBOR maxDatumSize
      <> toCBOR (Set.toAscList <$> allowedScriptAddresses)
      <> toCBOR (Set.toAscList <$> allowedDeposits)
   where
    allowedPolicies = case policyRule of
      Nothing -> Nothing
      Just ForbidAnyPolicy -> Just []
      Just (AllowOnlyPolicies policies) -> Just $ Set.toAscList policies

withPolicyAgreement ::
  (MonadAsync m, MonadDelay m, MonadLabelledSTM m) =>
  Tracer m PolicyLog ->
  Secret (SigningKey HydraKey) ->
  [Party] ->
  Maybe PolicyFingerprint ->
  Text ->
  PolicyStore m ->
  m () ->
  m ()
withPolicyAgreement _ _ _ Nothing _ _ action = action
withPolicyAgreement tracer signingKey otherParties (Just fingerprint) session PolicyStore{publish, load} action = do
  observed <- newLabelledTVarIO "peer-policies" Map.empty
  let initialStatus = (Set.toList expected, [])
      statusLog = \case
        ([], []) -> PolicyAgreementReached fingerprint
        (missingParties, mismatchedPolicies) -> PolicyAgreementPending{ourPolicy = fingerprint, missingParties, mismatchedPolicies}
      announce acknowledgements =
        let declaration = PolicyAnnouncement{session, fingerprint, acknowledgements}
         in publish $ Signed declaration (sign signingKey declaration) me
      poll previousAcknowledgements previousStatus = do
        declarations <- load
        let peers =
              Map.fromList
                [ (party, declaration)
                | Signed declaration signature party@Party{vkey} <- declarations
                , party `Set.member` expected
                , verify vkey signature declaration
                ]
            acknowledgements = Map.map (\PolicyAnnouncement{session = peerSession} -> peerSession) peers
            -- A saved declaration cannot acknowledge this startup's random session.
            fresh = Map.filter (\PolicyAnnouncement{acknowledgements = acks} -> Map.lookup me acks == Just session) peers
            status =
              ( Set.toList $ expected Set.\\ Map.keysSet fresh
              , [(party, peerPolicy) | (party, PolicyAnnouncement{fingerprint = peerPolicy}) <- Map.toList fresh, peerPolicy /= fingerprint]
              )
        when (status /= previousStatus) $ traceWith tracer $ statusLog status
        when (acknowledgements /= previousAcknowledgements) $ announce acknowledgements
        atomically $ writeTVar observed fresh
        threadDelay $ if status == ([], []) then 60 else 5
        poll acknowledgements status
      awaitAgreement = do
        atomically $ do
          peers <- readTVar observed
          check $ Map.keysSet peers == expected && all (\PolicyAnnouncement{fingerprint = peerPolicy} -> peerPolicy == fingerprint) peers
        action
  traceWith tracer $ statusLog initialStatus
  -- Keep responding after startup so a restarting peer can check us as well.
  either pure pure
    =<< raceLabelled
      ("publish-policy", announce Map.empty >> poll Map.empty initialStatus)
      ("await-policy-agreement", awaitAgreement)
 where
  me = deriveParty signingKey
  expected = Set.delete me $ Set.fromList otherParties
