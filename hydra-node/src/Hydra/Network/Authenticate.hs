module Hydra.Network.Authenticate where


import Hydra.Prelude
import Hydra.Crypto (Signature, Key (SigningKey), HydraKey, verify, sign)
import Cardano.Crypto.Util (SignableRepresentation)
import Hydra.Party (Party (Party, vkey))
import Hydra.Network (NetworkComponent, Network (Network, broadcast))

data Authenticated msg = Authenticated
  { payload :: msg
  , signature :: Signature msg
  }
  deriving (Eq, Show)

withAuthentication ::
  ( MonadAsync m
  , SignableRepresentation msg
  ) =>
  SigningKey HydraKey ->
  [Party] ->
  NetworkComponent m (Authenticated msg) a ->
  NetworkComponent m msg a
withAuthentication signingKey parties withRawNetwork callback action = do
  withRawNetwork checkSignature authenticate
 where
  checkSignature (Authenticated msg sig) = do
    when (any (\Party{vkey} -> verify vkey sig msg) parties) $
      callback msg
  authenticate = \Network{broadcast} ->
    action $ Network{broadcast = \msg -> broadcast (Authenticated msg (sign signingKey msg))}



