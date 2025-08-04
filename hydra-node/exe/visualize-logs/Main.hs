{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Main where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (encodeUtf8)
import Hydra.Prelude qualified as P

import Conduit
import Control.Lens ((^?))
import Data.Aeson (eitherDecode')
import Data.Aeson.Lens (key, _String)
import Data.ByteString.Lazy.Char8 qualified as C8
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import GHC.Show (show)
import Hydra.Chain (ChainEvent (..))
import Hydra.HeadLogic (Effect (..), Input (..), Outcome (..))
import Hydra.Logging (Envelope (..))
import Hydra.Logging.Messages (HydraLog (..))
import Hydra.Node (HydraNodeLog (..))

data Decoded tx
  = DecodedHydraLog Text
  | DropLog
  deriving (Eq)

instance Show (Decoded tx) where
  show (DecodedHydraLog txt) = T.unpack txt
  show DropLog = ""

main :: IO ()
main = do
  decodedLines <-
    runConduitRes $
      sourceFileBS "../devnet/alice-logs.txt"
        .| linesUnboundedAsciiC
        .| mapMC
          ( \l ->
              case l ^? key "message" . _String of
                Nothing -> P.error "Failed to find key 'message' which was expected"
                Just line ->
                  let envelope = fromStrict $ encodeUtf8 line
                   in case decodeAs envelope (undefined :: Envelope (HydraLog Tx)) of
                        Left e -> P.error $ P.show e <> line
                        Right decoded ->
                          case decoded.message of
                            NodeOptions opt -> pure $ DecodedHydraLog $ "NODE STARTING: \n" <> P.show opt
                            Node msg ->
                              case msg of
                                BeginInput{input} ->
                                  case input of
                                    ClientInput{clientInput} ->
                                      pure $ DecodedHydraLog $ "NODE LOG: \n" <> P.show clientInput
                                    NetworkInput{} -> pure DropLog
                                    ChainInput{chainEvent} ->
                                      case chainEvent of
                                        Observation{observedTx} -> pure $ DecodedHydraLog $ "OBESERVATION: " <> P.show observedTx
                                        Rollback{} -> pure DropLog
                                        Tick{} -> pure DropLog
                                        PostTxError{postTxError} ->
                                          pure $ DecodedHydraLog $ "ERROR: " <> P.show postTxError
                                EndInput{} -> pure DropLog
                                BeginEffect{effect} ->
                                  case effect of
                                    ClientEffect{} -> pure DropLog
                                    NetworkEffect{message} -> pure $ DecodedHydraLog $ P.show message
                                    OnChainEffect{postChainTx} -> pure $ DecodedHydraLog $ "POSTING: " <> P.show postChainTx
                                EndEffect{} -> pure DropLog
                                LogicOutcome{outcome} ->
                                  case outcome of
                                    Continue{} -> pure DropLog
                                    Wait{} -> pure DropLog
                                    Error{error = err} -> pure $ DecodedHydraLog $ "LOGIC ERROR: " <> P.show err
                                DroppedFromQueue{} -> pure DropLog
                                LoadingState -> pure $ DecodedHydraLog "Loading state..."
                                LoadedState{} -> pure $ DecodedHydraLog "Loaded."
                                ReplayingState -> pure $ DecodedHydraLog "Replaying state..."
                                Misconfiguration{} -> pure $ DecodedHydraLog "MISCONFIG!"
                            _ -> pure DropLog
          )
        .| filterC (\a -> P.show a /= ("" :: Text))
        .| sinkList
  forM_ decodedLines $ \l ->
    putTextLn (P.show l)

decodeAs :: forall a. FromJSON a => C8.ByteString -> a -> Either String a
decodeAs l _ =
  case eitherDecode' l :: Either String a of
    Left e -> Left e
    Right decoded -> pure decoded
