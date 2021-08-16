module Hydra.NodeSpec where

import Cardano.Crypto.DSIGN (SigDSIGN (SigMockDSIGN))
import Control.Exception (ErrorCall)
import Data.Sequence (Seq ((:|>)))
import qualified Data.Sequence as Seq
import Hydra.Chain (Chain (Chain), OnChainTx (..))
import Hydra.ClientInput (ClientInput (..))
import Hydra.HeadLogic (
  Environment (Environment),
  Event (..),
  HeadState (..),
  LogicError (InvalidSnapshot),
  SnapshotStrategy (..),
 )
import Hydra.Ledger (Party, Signed (UnsafeSigned), SigningKey, Tx, deriveParty)
import Hydra.Ledger.Simple (SimpleTx (..), simpleLedger, utxoRef, utxoRefs)
import Hydra.Network (Network (..))
import Hydra.Network.Message (Message (..))
import qualified Hydra.Network.Message as Msg
import Hydra.Node (HydraNode (..), createEventQueue, createHydraHead, processNextEvent)
import Hydra.Prelude
import Test.Hspec (Selector, Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property)
import Test.QuickCheck.Monadic (assert, monadicIO, run)

spec :: Spec
spec =
  describe "Hydra Node" $
    prop
      "throws an error given next snapshot is acked before current snapshot finishes"
      prop_unexpectedSnapshotDoesNotThrowError

prop_unexpectedSnapshotDoesNotThrowError :: Events -> Property
prop_unexpectedSnapshotDoesNotThrowError (Events evs) = monadicIO $ do
  result <- run $ do
    node <- createHydraNode 20 [30, 10] SnapshotAfterEachTx
    feedEvents node (prefix <> toList evs)

  assert $ isNotErrorCall result

isNotErrorCall :: Maybe (FeedError SimpleTx) -> Bool
isNotErrorCall Nothing = True
isNotErrorCall (Just (FeedError [])) = False
isNotErrorCall (Just (FeedError _)) = True

newtype Events = Events (Seq (Event SimpleTx))
  deriving (Eq, Show)

instance Arbitrary Events where
  arbitrary = pure (Events $ Seq.fromList reducedEvents)

  shrink = \case
    Events (evs :|> ev :|> ev') -> [Events (evs :|> ev), Events (evs :|> ev')]
    _ -> []

prefix :: [Event SimpleTx]
prefix =
  [ NetworkEvent{message = Connected{Msg.party = 30}}
  , NetworkEvent{message = Connected{Msg.party = 10}}
  , OnChainEvent
      { onChainTx = OnInitTx 10 [10, 20, 30]
      }
  , ClientEvent{clientInput = Commit (utxoRef 2)}
  , OnChainEvent{onChainTx = OnCommitTx 30 (utxoRef 3)}
  , OnChainEvent{onChainTx = OnCommitTx 20 (utxoRef 2)}
  , OnChainEvent{onChainTx = OnCommitTx 10 (utxoRef 1)}
  , OnChainEvent{onChainTx = OnCollectComTx}
  ]

reducedEvents :: [Event SimpleTx]
reducedEvents =
  [ NetworkEvent{message = ReqTx{Msg.party = 10, transaction = SimpleTx{txSimpleId = 1, txInputs = utxoRefs [2], txOutputs = utxoRefs [4, 5, 6, 7, 8, 9, 10, 11, 12, 13]}}}
  , NetworkEvent{message = ReqSn{Msg.party = 10, Msg.snapshotNumber = 1, transactions = [SimpleTx{txSimpleId = 1, txInputs = utxoRefs [2], txOutputs = utxoRefs [4, 5, 6, 7, 8, 9, 10, 11, 12, 13]}]}}
  , NetworkEvent{message = AckSn{Msg.party = 10, signed = UnsafeSigned (SigMockDSIGN "b02393e9d7ded6c0" 10), Msg.snapshotNumber = 1}}
  , NetworkEvent{message = AckSn{Msg.party = 20, signed = UnsafeSigned (SigMockDSIGN "b02393e9d7ded6c0" 20), Msg.snapshotNumber = 1}}
  , NetworkEvent{message = AckSn{Msg.party = 30, signed = UnsafeSigned (SigMockDSIGN "b02393e9d7ded6c0" 30), Msg.snapshotNumber = 1}}
  , NetworkEvent{message = ReqTx{Msg.party = 10, transaction = SimpleTx{txSimpleId = 2, txInputs = utxoRefs [1, 3, 4, 5, 6, 8, 9, 10, 11, 13], txOutputs = utxoRefs [14, 15]}}}
  , NetworkEvent{message = ReqTx{Msg.party = 10, transaction = SimpleTx{txSimpleId = 3, txInputs = utxoRefs [7, 14], txOutputs = utxoRefs [16, 17, 18, 19, 20, 21, 22, 23]}}}
  , NetworkEvent{message = ReqTx{Msg.party = 10, transaction = SimpleTx{txSimpleId = 4, txInputs = utxoRefs [17, 18, 20, 21], txOutputs = utxoRefs [24, 25]}}}
  , NetworkEvent{message = AckSn{Msg.party = 30, signed = UnsafeSigned (SigMockDSIGN "c1a00d60aabde67a" 30), Msg.snapshotNumber = 2}}
  , NetworkEvent{message = ReqSn{Msg.party = 10, Msg.snapshotNumber = 2, transactions = [SimpleTx{txSimpleId = 2, txInputs = utxoRefs [1, 3, 4, 5, 6, 8, 9, 10, 11, 13], txOutputs = utxoRefs [14, 15]}, SimpleTx{txSimpleId = 3, txInputs = utxoRefs [7, 14], txOutputs = utxoRefs [16, 17, 18, 19, 20, 21, 22, 23]}, SimpleTx{txSimpleId = 4, txInputs = utxoRefs [17, 18, 20, 21], txOutputs = utxoRefs [24, 25]}]}}
  , NetworkEvent{message = AckSn{Msg.party = 30, signed = UnsafeSigned (SigMockDSIGN "7d81c3daff6fe561" 30), Msg.snapshotNumber = 3}}
  ]

events :: Events
events =
  Events $
    Seq.fromList
      [ NetworkEvent
          { message =
              ReqTx
                { Msg.party = 10
                , transaction =
                    SimpleTx
                      { txSimpleId = 1
                      , txInputs = utxoRef 2
                      , txOutputs = utxoRefs [4, 5, 6, 7, 8, 9, 10, 11, 12, 13]
                      }
                }
          }
      , NetworkEvent
          { message =
              ReqSn
                { Msg.party = 10
                , Msg.snapshotNumber = 1
                , transactions =
                    [ SimpleTx
                        { txSimpleId = 1
                        , txInputs = utxoRef 2
                        , txOutputs = utxoRefs [4, 5, 6, 7, 8, 9, 10, 11, 12, 13]
                        }
                    ]
                }
          }
      , NetworkEvent
          { message =
              AckSn
                { Msg.party = 10
                , signed = UnsafeSigned (SigMockDSIGN "b02393e9d7ded6c0" 10)
                , Msg.snapshotNumber = 1
                }
          }
      , NetworkEvent
          { message =
              AckSn
                { Msg.party = 20
                , signed = UnsafeSigned (SigMockDSIGN "b02393e9d7ded6c0" 20)
                , Msg.snapshotNumber = 1
                }
          }
      , NetworkEvent
          { message =
              AckSn
                { Msg.party = 30
                , signed = UnsafeSigned (SigMockDSIGN "b02393e9d7ded6c0" 30)
                , Msg.snapshotNumber = 1
                }
          }
      , NetworkEvent
          { message =
              ReqTx
                { Msg.party = 10
                , transaction = SimpleTx{txSimpleId = 2, txInputs = utxoRefs [1, 3, 4, 5, 6, 8, 9, 10, 11, 13], txOutputs = utxoRefs [14, 15]}
                }
          }
      , NetworkEvent
          { message =
              ReqTx
                { Msg.party = 10
                , transaction = SimpleTx{txSimpleId = 3, txInputs = utxoRefs [7, 14], txOutputs = utxoRefs [16, 17, 18, 19, 20, 21, 22, 23]}
                }
          }
      , NetworkEvent
          { message =
              ReqTx
                { Msg.party = 10
                , transaction = SimpleTx{txSimpleId = 4, txInputs = utxoRefs [17, 18, 20, 21], txOutputs = utxoRefs [24, 25]}
                }
          }
      , NetworkEvent
          { message =
              AckSn
                { Msg.party = 30
                , signed = UnsafeSigned (SigMockDSIGN "c1a00d60aabde67a" 30)
                , Msg.snapshotNumber = 2
                }
          }
      , NetworkEvent
          { message =
              ReqSn
                { Msg.party = 10
                , Msg.snapshotNumber = 2
                , transactions =
                    [ SimpleTx
                        { txSimpleId = 2
                        , txInputs = utxoRefs [1, 3, 4, 5, 6, 8, 9, 10, 11, 13]
                        , txOutputs = utxoRefs [14, 15]
                        }
                    , SimpleTx{txSimpleId = 3, txInputs = utxoRefs [7, 14], txOutputs = utxoRefs [16, 17, 18, 19, 20, 21, 22, 23]}
                    , SimpleTx{txSimpleId = 4, txInputs = utxoRefs [17, 18, 20, 21], txOutputs = utxoRefs [24, 25]}
                    ]
                }
          }
      , NetworkEvent
          { message =
              AckSn
                { Msg.party = 10
                , signed = UnsafeSigned (SigMockDSIGN "c1a00d60aabde67a" 10)
                , Msg.snapshotNumber = 2
                }
          }
      , NetworkEvent
          { message =
              AckSn
                { Msg.party = 20
                , signed = UnsafeSigned (SigMockDSIGN "c1a00d60aabde67a" 20)
                , Msg.snapshotNumber = 2
                }
          }
      , NetworkEvent
          { message =
              ReqTx
                { Msg.party = 10
                , transaction = SimpleTx{txSimpleId = 5, txInputs = utxoRefs [12, 15, 22], txOutputs = utxoRefs [26, 27, 28]}
                }
          }
      , NetworkEvent
          { message =
              ReqTx
                { Msg.party = 10
                , transaction = SimpleTx{txSimpleId = 6, txInputs = utxoRefs [16, 19, 23, 24, 25], txOutputs = utxoRefs [29]}
                }
          }
      , NetworkEvent
          { message =
              ReqTx
                { Msg.party = 10
                , transaction = SimpleTx{txSimpleId = 7, txInputs = utxoRefs [26, 28, 29], txOutputs = utxoRefs [30, 31, 32, 33, 34, 35, 36]}
                }
          }
      , NetworkEvent
          { message =
              ReqTx
                { Msg.party = 10
                , transaction = SimpleTx{txSimpleId = 8, txInputs = utxoRefs [30, 33, 34], txOutputs = utxoRefs [37, 38]}
                }
          }
      , NetworkEvent
          { message =
              ReqTx
                { Msg.party = 10
                , transaction = SimpleTx{txSimpleId = 9, txInputs = utxoRefs [27, 32, 36, 37, 38], txOutputs = utxoRefs [39, 40, 41]}
                }
          }
      , NetworkEvent
          { message =
              ReqTx
                { Msg.party = 10
                , transaction = SimpleTx{txSimpleId = 10, txInputs = utxoRefs [31, 35], txOutputs = utxoRefs [42, 43, 44, 45, 46, 47]}
                }
          }
      , NetworkEvent
          { message =
              ReqSn
                { Msg.party = 10
                , Msg.snapshotNumber = 3
                , transactions =
                    [ SimpleTx
                        { txSimpleId = 5
                        , txInputs = utxoRefs [12, 15, 22]
                        , txOutputs = utxoRefs [26, 27, 28]
                        }
                    , SimpleTx{txSimpleId = 6, txInputs = utxoRefs [16, 19, 23, 24, 25], txOutputs = utxoRefs [29]}
                    , SimpleTx{txSimpleId = 7, txInputs = utxoRefs [26, 28, 29], txOutputs = utxoRefs [30, 31, 32, 33, 34, 35, 36]}
                    , SimpleTx{txSimpleId = 8, txInputs = utxoRefs [30, 33, 34], txOutputs = utxoRefs [37, 38]}
                    ]
                }
          }
      , NetworkEvent
          { message =
              AckSn
                { Msg.party =
                    30
                , signed = UnsafeSigned (SigMockDSIGN "7d81c3daff6fe561" 30)
                , Msg.snapshotNumber = 3
                }
          }
      ]

-- | "Prepare" a node by feeding it a list of 'Event's.
-- returns Nothing if no event is an error, or 'Just FeedError' if there's
-- any error.
feedEvents ::
  ( Tx tx
  , Traversable t
  ) =>
  HydraNode tx IO ->
  t (Event tx) ->
  IO (Maybe (FeedError tx))
feedEvents node eventsList =
  atomically
    ( do
        (errs, _) <- partitionEithers . toList <$> traverse (processNextEvent node) eventsList
        if null errs
          then pure Nothing
          else pure $ Just $ FeedError errs
    )
    `catch` \(_ :: ErrorCall) -> pure (Just $ FeedError [])

newtype FeedError tx = FeedError [LogicError tx]
  deriving stock (Eq, Show, Generic)

instance Tx tx => Exception (FeedError tx)

aLogicError :: Selector (LogicError SimpleTx)
aLogicError = const True

anInvalidSnapshotError :: Selector (LogicError SimpleTx)
anInvalidSnapshotError = \case
  InvalidSnapshot{} -> True
  _ -> False

createHydraNode :: MonadSTM m => SigningKey -> [Party] -> SnapshotStrategy -> m (HydraNode SimpleTx m)
createHydraNode signingKey otherParties snapshotStrategy = do
  let env =
        Environment
          party
          signingKey
          otherParties
          snapshotStrategy

  eq <- createEventQueue
  hh <- createHydraHead ReadyState simpleLedger
  let hn' = Network{broadcast = const $ pure ()}
  pure $
    HydraNode
      { eq
      , hn = hn'
      , hh
      , oc = Chain (const $ pure ())
      , sendOutput = const $ pure ()
      , env
      }
 where
  party = deriveParty signingKey
