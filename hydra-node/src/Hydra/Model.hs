{-# LANGUAGE AllowAmbiguousTypes #-}

-- | A first take on our problem domain and how we would model things. This is a
-- DRAFT and most things are likely incomplete.
module Hydra.Model where

import Cardano.Prelude
import Network.TypedProtocol.FireForget.Server (FireForgetServer)

data Tx

data ClientCommand
  = Init
  | Commit
  | NewTx
  | Close
  | Contest

data ClientInstruction
  = InitTxToSign
  | CommitTxDraft
  | HeadClosed
  | TxConfirmed

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

data Output
  = ClientOutput ClientInstruction
  | HydraOutput HydraMessage
  | OnChainOutput OnChainTx

data HeadState

-- | The heart of the Hydra head protocol, a handler of all kinds of 'Event' in
-- the Hydra head. This may also be split into multiple handlers, i.e. one for
-- hydra network events, one for client events and one for main chain events.
hydraHeadProtocolHandler :: HeadState -> Event -> (HeadState, [Output])
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

-- | Contains at least the contestation period and other things.
data HeadParameters

-- | Decides when, how often and who is in charge of creating snapshots.
data SnapshotStrategy

-- | Assume: We know the party members and their verification keys. These need
-- to be exchanged somehow, eventually.
createHeadState :: [Party] -> HeadParameters -> SnapshotStrategy -> HeadState
createHeadState = panic "not implemented"

-- * Ledger stuff

class HydraLedger l where
  type LedgerState l :: Type

  applyTxs ::
    LedgerEnv ->
    LedgerState l ->
    Seq Tx ->
    Either ValidationError (LedgerState l)

validateTx :: HydraLedger l => LedgerState l -> Tx -> ValidationResult
validateTx = panic "derive this from applyTx"

data LedgerEnv = LedgerEnv
  { protocolParameters :: ProtocolParameters
  , currentSlot :: Natural
  -- and other things: epochInfo, securityParameter, networkId ...
  }

-- | A set of unspent transaction outputs.
data UTxO

-- | Mainchain protocol parameters used mainly for validating transactions (in place of the mainchain).
data ProtocolParameters

-- | Our (subset of a) ledger state, which contains (at least) the utxo set and
-- mainchain protocol parameters.
-- type LedgerState = UTxO

-- | Validate a transaction given a ledger state and mainchain protocol
-- parameters. TODO: required if we have applyTx?
-- validateTx :: LedgerState -> Tx -> ValidationResult
-- validateTx = panic "not implemented"

-- | Validate and apply a transaction if valid to a given ledger.
-- applyTx :: ProtocolParameters -> LedgerState -> Tx -> Either ValidationError LedgerState
-- applyTx = panic "not implemented"

-- * Too detailed stuff

main :: IO ()
main = do
  hn <- panic "undefined"
  mc <- panic "undefined"
  hh <- panic "undefined"

  forever $ do
    e <- getEvent
    runHydraHeadProtocol hn mc hh e

-- | Connects to a configured set of peers and sets up the whole network stack.
setupHydraNetwork :: MainChain IO -> HydraHead IO -> IO (HydraNetworkClient IO)
setupHydraNetwork mc hh = do
  let client = panic "create HydraNetworkClient interface out of ourobouros client"
  let server = hydraMessageServer (runHydraHeadProtocol client mc hh . HydraEvent)
  -- ourobouros network stuff, setting up mux, protocols and whatnot
  panic "not implemented"

hydraMessageServer :: (HydraMessage -> IO ()) -> FireForgetServer HydraMessage IO ()
hydraMessageServer action = do
  -- FireForgetServer
  --   { recvMsg = \msg -> action msg >> hydraMessageServer
  --   , recvMsgDone = putTextLn "Done."
  --   }
  panic "not implemented"

-- | Monadic interface around 'hydraHeadProtocolHandler'.
runHydraHeadProtocol :: Monad m => HydraNetworkClient m -> MainChain m -> HydraHead m -> Event -> m ()
runHydraHeadProtocol HydraNetworkClient{broadcast} MainChain{postTx} HydraHead{getState, putState} e = do
  -- s <- getState
  -- let (s', msgs, txs) = hydraHeadProtocolHandler s e
  -- mapM_ broadcast msgs
  -- mapM_ postTx txs
  -- putState s'
  panic "not implemented"

data HydraHead m = HydraHead
  { getState :: m HeadState
  , putState :: HeadState -> m ()
  }

newtype HydraNetworkClient m = HydraNetworkClient
  { broadcast :: HydraMessage -> m ()
  }

newtype MainChain m = MainChain
  { postTx :: OnChainTx -> m ()
  }
