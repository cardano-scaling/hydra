module Hydra.Tx.Crypto.MultiSignature where

import Cardano.Crypto.Util (SignableRepresentation, getSignableRepresentation)
import Data.Map.Strict qualified as Map
import Hydra.Prelude
import Hydra.Tx.Crypto.Signature (HydraKey, Signature, VerificationKey, verifyBytes)

newtype MultiSignature (a :: Type) = HydraMultiSignature {multiSignature :: [Signature a]}
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid)

instance Typeable a => ToCBOR (MultiSignature a) where
  toCBOR = genericToCBOR

instance Typeable a => FromCBOR (MultiSignature a) where
  fromCBOR = genericFromCBOR

deriving anyclass instance ToJSON a => ToJSON (MultiSignature a)
deriving anyclass instance FromJSON a => FromJSON (MultiSignature a)

aggregate :: [Signature a] -> MultiSignature a
aggregate = HydraMultiSignature

aggregateInOrder :: Ord k => Map k (Signature a) -> [k] -> MultiSignature a
aggregateInOrder signatures = HydraMultiSignature . foldr appendSignature []
 where
  appendSignature k sigs =
    case Map.lookup k signatures of
      Nothing -> sigs
      Just sig -> sig : sigs

data VerificationError
  = InvalidSignatures (NonEmpty (VerificationKey HydraKey))
  | SignatureCountMismatch
  deriving stock (Eq, Show, Generic)

verifyMultiSignature ::
  SignableRepresentation a =>
  [VerificationKey HydraKey] ->
  MultiSignature a ->
  a ->
  Either VerificationError ()
verifyMultiSignature vks multisig a =
  verifyMultiSignatureBytes vks multisig (getSignableRepresentation a)

verifyMultiSignatureBytes ::
  [VerificationKey HydraKey] ->
  MultiSignature a ->
  ByteString ->
  Either VerificationError ()
verifyMultiSignatureBytes vks HydraMultiSignature{multiSignature} bs
  | length vks == length multiSignature =
      let failures = [vk | (vk, signature) <- zip vks multiSignature, not (verifyBytes vk signature bs)]
       in case failures of
            [] -> Right ()
            failed : rest -> Left $ InvalidSignatures (failed :| rest)
  | otherwise = Left SignatureCountMismatch
