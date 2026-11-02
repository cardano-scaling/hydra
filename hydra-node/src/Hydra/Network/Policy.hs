module Hydra.Network.Policy where

import Hydra.Prelude

import Cardano.Binary (serialize')
import Cardano.Crypto.Hash (Blake2b_256, hashToStringAsHex, hashWithSerialiser)
import Cardano.Crypto.Util (SignableRepresentation (..))
import Codec.CBOR.Encoding (Encoding)
import Control.Concurrent.Class.MonadSTM (check, readTVarIO, writeTVar)
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
  deriving newtype (ToJSON, FromJSON, ToCBOR, FromCBOR)

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

data PolicyAgreementException = PolicyAgreementTimedOut
  { ourPolicy :: PolicyFingerprint
  , missingParties :: [Party]
  , mismatchedPolicies :: [(Party, PolicyFingerprint)]
  }
  deriving stock (Eq, Show)

instance Exception PolicyAgreementException where
  displayException PolicyAgreementTimedOut{ourPolicy = PolicyFingerprint fingerprint, missingParties, mismatchedPolicies} =
    "Policy agreement timed out after five minutes. Local policy: "
      <> toString fingerprint
      <> ". Parties without a fresh acknowledgement: "
      <> show missingParties
      <> ". Parties using a different policy: "
      <> show mismatchedPolicies

data PolicyLog
  = WaitingForPolicyAgreement PolicyFingerprint [Party]
  | PolicyAgreementReached PolicyFingerprint
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

policyFingerprint :: Maybe UserValidationRules -> PolicyFingerprint
policyFingerprint =
  PolicyFingerprint . toText . hashToStringAsHex . hashWithSerialiser @Blake2b_256 encode . fromMaybe unrestricted
 where
  unrestricted = UserValidationRules Nothing AllowMinting Nothing Nothing Nothing Nothing Nothing

  -- Fixed field order and sorted sets make this independent of YAML formatting.
  -- Change the domain version if the encoding or interpretation changes.
  encode :: UserValidationRules -> Encoding
  encode UserValidationRules{policyRule, l2MintingRule, maxUTxOCount, maxOutputValueAssets, maxDatumSize, allowedScriptAddresses, allowedDeposits} =
    toCBOR ("hydra-policy-v1" :: Text)
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
  (MonadAsync m, MonadTimer m, MonadDelay m, MonadMask m, MonadLabelledSTM m) =>
  Tracer m PolicyLog ->
  Secret (SigningKey HydraKey) ->
  [Party] ->
  PolicyFingerprint ->
  Text ->
  PolicyStore m ->
  m a ->
  m a
withPolicyAgreement tracer signingKey otherParties fingerprint session PolicyStore{publish, load} action = do
  observed <- newLabelledTVarIO "peer-policies" Map.empty
  traceWith tracer $ WaitingForPolicyAgreement fingerprint (Set.toList expected)
  let announce acknowledgements =
        let declaration = PolicyAnnouncement{session, fingerprint, acknowledgements}
         in publish $ Signed declaration (sign signingKey declaration) me
      poll previous = do
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
        when (acknowledgements /= previous) $ announce acknowledgements
        atomically $ writeTVar observed fresh
        threadDelay 1
        poll acknowledgements
      awaitAgreement = do
        result <- timeout 300 $ atomically $ do
          peers <- readTVar observed
          check $ Map.keysSet peers == expected && all (\PolicyAnnouncement{fingerprint = peerPolicy} -> peerPolicy == fingerprint) peers
        case result of
          Just () -> traceWith tracer (PolicyAgreementReached fingerprint) >> action
          Nothing -> do
            peers <- readTVarIO observed
            throwIO
              PolicyAgreementTimedOut
                { ourPolicy = fingerprint
                , missingParties = Set.toList $ expected Set.\\ Map.keysSet peers
                , mismatchedPolicies = [(party, peerPolicy) | (party, PolicyAnnouncement{fingerprint = peerPolicy}) <- Map.toList peers, peerPolicy /= fingerprint]
                }
  -- Keep responding after startup so a restarting peer can check us as well.
  either pure pure
    =<< raceLabelled
      ("publish-policy", announce Map.empty >> poll Map.empty)
      ("await-policy-agreement", awaitAgreement)
 where
  me = deriveParty signingKey
  expected = Set.delete me $ Set.fromList otherParties
