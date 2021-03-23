module Hydra.Model where

import Cardano.Prelude

data Tx

data ClientCommand
  = Init
  | Commit
  | NewTx
  | Close
  | Contest

-- | Messages which are sent through the hydra node-to-node network
data HydraMessage
  = ReqTx
  | AckTx
  | ConfTx
  | ReqSn
  | AckSn

data OnChainTx
  = InitTx
  | CommitTx
  | CollectComTx
  | CloseTx
  | ContestTx
  | FanoutTx

data Event
  = ClientEvent ClientCommand
  | HydraEvent HydraMessage
  | OnChainEvent OnChainTx

data HeadState

-- | The heart of the Hydra head protocol, a handler of all kinds of 'Event' in
-- the Hydra head. This may also be split into multiple handlers, i.e. one for
-- hydra network events, one for client events and one for main chain events.
hydraHeadProtocolHandler :: HeadState -> Event -> (HeadState, [HydraMessage], [OnChainTx])
hydraHeadProtocolHandler = panic "not implemented"

-- | Verification used to authenticate main chain transactions that are
-- restricted to members of the Head protocol instance, i.e. the commit
-- transaction. This key is named k_i in the paper and for Cardano, this is
-- currently a Ed25519 verification key
data OnChainVerificationKey

-- | Verification key to the signing key used for signing / acking transactions
-- off chain. This key is named K_i in the paper and can be aggregated with
-- other party member's 'HydraVerificationKey' to K_agg.
data HydraVerificationKey

-- | Identifes a party in a Hydra head.
type Party = (OnChainVerificationKey, HydraVerificationKey)

-- alice :: Party
-- bob :: Party
-- carol :: Party

data HeadParameters

-- | Assume: We know the party members and their verification keys. These need
-- to be exchanged somehow, eventually.
initHead :: [Party] -> HeadParameters -> HeadState
initHead = panic "not implemented"

-- | Either valid or an error which we get from the ledger-specs tx validation.
data ValidationResult

-- | A set of unspent transaction outputs.
data UTxO

-- | Mainchain protocol parameters used mainly for validating transactions (in place of the mainchain).
data ProtocolParameters

-- | Our (subset of a) ledger state, which contains (at least) the utxo set and
-- mainchain protocol parameters.
type LedgerState = (UTxO, ProtocolParameters)

-- | Validate a transaction given a ledger state and mainchain protocol parameters.
validateTx :: LedgerState -> Tx -> ValidationResult
validateTx = panic "not implemented"
