{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Top-level module to run a single Hydra node
module Hydra.Node where

import Cardano.Prelude
import qualified Data.Map.Strict as Map hiding (map)
import qualified Data.Set as Set
import Hydra.ContractStateMachine (
  Eta (..),
  HydraState (..),
  MultisigPublicKey (..),
  OpenState (..),
  UTXO (..),
  VerificationKey (..),
  contractAddress,
  toDatumHash,
 )
import Hydra.MonetaryPolicy (hydraCurrencySymbol)
import qualified Ledger as Plutus
import qualified Ledger.Value as Value

runHydra :: IO ()
runHydra =
  startStopNode $
    const $ do
      putStrLn ("Hydra started" :: Text)
      waitForInput
 where
  waitForInput = void getLine

-- | Current state of a Hydra node
newtype HydraNode = HydraNode {nodeThread :: Maybe (Async ())}

-- | High-level lifecycle states for a Hydra node
data NodeStatus = NotReady | Ready
  deriving stock (Eq, Show)

startNode :: IO ()
startNode = forever $ threadDelay oneSecond

oneSecond :: Int
oneSecond = 1_000_000

stopNode :: HydraNode -> IO HydraNode
stopNode node =
  case nodeThread node of
    Just thread -> cancel thread >> pure (HydraNode Nothing)
    Nothing -> pure node

startStopNode :: (HydraNode -> IO ()) -> IO ()
startStopNode act = withAsync startNode $ \thread -> do
  let node = HydraNode (Just thread)
  res <- act node
  void $ stopNode node
  pure res

status :: HydraNode -> IO NodeStatus
status node =
  case nodeThread node of
    Just{} -> pure Ready
    Nothing -> pure NotReady

buildInitialTransaction :: HydraNode -> HeadParameters -> IO (Transaction, PolicyId)
buildInitialTransaction _ HeadParameters{verificationKeys, monetaryPolicyInput} = do
  let policyId = fromCurrencySymbol $ hydraCurrencySymbol (outputRef monetaryPolicyInput)
  let mkOutput verificationKey =
        TransactionOutput
          { value =
              Value
                (Quantity 0)
                (Map.fromList [(policyId, Map.fromList [(AssetName (unverificationKey verificationKey), Quantity 1)])])
          , address = PubKeyAddress verificationKey
          , datum = Nothing
          }
  let stateMachineOutput =
        TransactionOutput
          { value = Value 0 mempty
          , address = ScriptAddress contractAddress
          , datum = Just (toDatumHash (initialState verificationKeys))
          }
  let outputs = stateMachineOutput : map mkOutput verificationKeys
  let inputs = [monetaryPolicyInput]
  return (Transaction{outputs, inputs}, policyId)

data Transaction = Transaction
  { outputs :: [TransactionOutput]
  , inputs :: [TransactionInput]
  }

assetNames :: PolicyId -> Transaction -> Set AssetName
assetNames policyId =
  Set.fromList
    . concatMap Map.keys
    . mapMaybe (Map.lookup policyId . tokens . value)
    . outputs

getStateMachineOutput :: Transaction -> Maybe TransactionOutput
getStateMachineOutput = find (isJust . datum) . outputs

data TransactionOutput = TransactionOutput
  { value :: Value
  , address :: Address
  , datum :: Maybe Plutus.DatumHash
  }

data Value = Value
  { adas :: Quantity
  , tokens :: Map PolicyId (Map AssetName Quantity)
  }

data Address
  = PubKeyAddress VerificationKey
  | ScriptAddress Plutus.Address

toPlutusAddress :: Address -> Maybe Plutus.Address
toPlutusAddress = \case
  ScriptAddress addr -> Just addr
  _ -> Nothing

newtype Quantity = Quantity Natural
  deriving newtype (Eq, Num)

data TransactionInput = TransactionInput {txId :: Plutus.TxId, utxoIndex :: Integer}

mkTransactionInput :: ByteString -> Integer -> TransactionInput
mkTransactionInput txRef idx = TransactionInput (Plutus.TxId txRef) idx

outputRef :: TransactionInput -> (Plutus.TxId, Integer)
outputRef TransactionInput{txId, utxoIndex} = (txId, utxoIndex)

toCurrencySymbol :: PolicyId -> Plutus.CurrencySymbol
toCurrencySymbol (PolicyId hash) = Value.currencySymbol hash

fromCurrencySymbol :: Plutus.CurrencySymbol -> PolicyId
fromCurrencySymbol = PolicyId . Value.unCurrencySymbol

data HeadParameters = HeadParameters
  { verificationKeys :: [VerificationKey]
  , monetaryPolicyInput :: TransactionInput
  }

data MonetaryScript
  = AnyOf [VerificationKey]

type AssetId = (PolicyId, AssetName)

-- Other constructors (e.g. AllOf, MOfN are left aside for now).

-- FIXME:
--
-- newtype PolicyId = Hash MonetaryScript Blake2b_224
--
-- newtype Hash (what :: Type) (alg :: Type) = Digest alg
newtype PolicyId = PolicyId ByteString deriving (Eq, Ord)

newtype AssetName = AssetName ByteString deriving (Eq, Ord)

--
-- Hydra State
--

initialState :: [VerificationKey] -> HydraState
initialState keys = Open openState
 where
  openState =
    OpenState
      { keyAggregate = MultisigPublicKey keys
      , eta =
          Eta
            { utxos = UTXO
            , snapshotNumber = 0
            , transactions = mempty
            }
      }
