{-# LANGUAGE AllowAmbiguousTypes #-}

-- | A first take on our problem domain and how we would model things. This is a
-- DRAFT and most things are likely incomplete.
module Hydra.Model where

import Cardano.Prelude
import Control.Concurrent.STM (TChan, newTChanIO, newTVarIO, readTChan, readTVarIO, writeTVar)
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
  deriving (Show)

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
  | NetworkEvent HydraMessage
  | OnChainEvent OnChainTx

data Output
  = ClientOutput ClientInstruction
  | NetworkOutput HydraMessage
  | OnChainOutput OnChainTx

data HeadState = HeadState

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

-- NOTE(SN): this output stuff is ugly as hell and I never would implement it
-- really like this ._.

type OutputQueue = TChan Event

putOutput :: OutputQueue -> Output -> IO ()
putOutput = panic "undefined"

getOutputIf :: OutputQueue -> (Output -> Bool) -> IO (Maybe Output)
getOutputIf = panic "undefined"

isOnChainOutput :: Output -> Bool
isOnChainOutput OnChainOutput{} = True
isOnChainOutput _ = False

isNetworkOutput :: Output -> Bool
isNetworkOutput OnChainOutput{} = True
isNetworkOutput _ = False

main :: IO ()
main = do
  eq <- newTChanIO
  hh <- setupHydraHead
  setupChainClient eq hh
  setupHydraNetwork eq hh
  -- TODO wait until killed?
  forever $ threadDelay 10000
 where
  -- TODO forM_ [0..1] $ async $
  -- runHydraHeadProtocol eq hn oc hh

  getEvent = panic "not implememted"

setupHydraHead :: IO (HydraHead IO)
setupHydraHead = do
  tv <- newTVarIO HeadState
  pure
    HydraHead
      { getState = readTVarIO tv
      , putState = atomically . writeTVar tv
      }

-- | Connects to a cardano node and sets up things in order to be able to
-- construct actual transactions using 'OnChainTx' and send them on 'postTx'.
setupChainClient :: OutputQueue -> HydraHead IO -> IO ()
setupChainClient oq hh = do
  let plutussChainSyncServer = runHydraHeadProtocol oq hh . OnChainEvent
  forever $
    getOutputIf oq isOnChainOutput >>= \case
      Just (OnChainOutput o) -> panic "construct and send transction e.g. using plutus from output"
      _ -> pure ()

-- | Connects to a configured set of peers and sets up the whole network stack.
setupHydraNetwork :: OutputQueue -> HydraHead IO -> IO (HydraNetwork IO)
setupHydraNetwork oq hh = do
  -- ourobouros network stuff, setting up mux, protocols and whatnot and wire in
  -- the 'server' and 'client' in an 'app' (like in the fire and forget example)
  panic "not implemented"
 where
  -- The server reacts on received 'HydraMessage' by running th protocol handler
  server = hydraMessageServer (runHydraHeadProtocol oq hh . NetworkEvent)

  -- The client endlessly reads from the output queue to send messages into the
  -- network
  client :: MonadIO m => m ()
  client =
    forever $
      liftIO (getOutputIf oq isNetworkOutput) >>= \case
        Just (NetworkOutput msg) -> sendMsg msg
        _ -> pure ()

  -- TODO(SN): this should be "the monadic interface" of SendMsg :: FireForgetClient msg m a
  sendMsg :: HydraMessage -> m ()
  sendMsg = panic "not implemented"

hydraMessageServer :: (HydraMessage -> IO ()) -> FireForgetServer HydraMessage IO ()
hydraMessageServer action = do
  -- FireForgetServer
  --   { recvMsg = \msg -> action msg >> hydraMessageServer
  --   , recvMsgDone = putTextLn "Done."
  --   }
  panic "not implemented"

-- | Monadic interface around 'hydraHeadProtocolHandler'.
runHydraHeadProtocol ::
  (Monad m, MonadIO m, Show ClientInstruction) =>
  OutputQueue ->
  HydraHead m ->
  Event ->
  m ()
runHydraHeadProtocol oq HydraHead{getState, putState} e =
  forever $ do
    -- e <- liftIO $ atomically $ readTChan eq
    s <- getState -- NOTE(SN): this is obviously not thread-safe
    let (s', out) = hydraHeadProtocolHandler s e
    putState s'
    forM_ out $ putOutput oq

-- | Handle to access and modify a Hydra Head's state.
data HydraHead m = HydraHead
  { getState :: m HeadState
  , putState :: HeadState -> m ()
  }

-- | Handle to interface with the hydra network and send messages "off chain".
newtype HydraNetwork m = HydraNetwork
  { -- | Send a 'HydraMessage' to the whole hydra network.
    broadcast :: HydraMessage -> m ()
  }

-- | Handle to interface with the main chain network
newtype OnChain m = OnChain
  { -- | Construct and send a transaction to the main chain corresponding to the
    -- given 'OnChainTx' event.
    postTx :: OnChainTx -> m ()
  }
