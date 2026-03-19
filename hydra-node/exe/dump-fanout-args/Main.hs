-- | Dump fanout script arguments for Plutus team evaluation.
--
-- This executable generates representative fanout transactions at two critical
-- UTxO sizes (28 = new memory ceiling after compiler 1.56, 47 = old ceiling)
-- and writes the transaction, UTxO, datum, and redeemer to JSON files.
--
-- The Plutus team can use these to evaluate both old and new vHead.plutus UPLC
-- programs with identical inputs to isolate the cost regression in the
-- @foldMap (serialiseData . toBuiltinData)@ hot path.
--
-- Output files per N:
--   fanout-N{n}-tx.json       - transaction (cardano-cli text envelope format)
--   fanout-N{n}-utxo.json     - reference UTxO (cardano-api JSON)
--   fanout-N{n}-datum.json    - ClosedDatum as PlutusData (detailed schema)
--   fanout-N{n}-redeemer.json - Fanout redeemer as PlutusData (detailed schema)
module Main where

import Hydra.Cardano.Api
import Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (fromJust, listToMaybe)
import Hydra.Chain.Direct.State (
  ClosedState (..),
  OpenState (..),
  ctxContestationPeriod,
  ctxHeadParameters,
  getKnownUTxO,
  observeClose,
  unsafeClose,
  unsafeFanout,
 )
import Hydra.Contract.HeadState qualified as Head
import Hydra.Ledger.Cardano.Time (slotNoFromUTCTime)
import PlutusLedgerApi.V3 (toData)
import Test.Hydra.Chain.Direct.State (
  genHydraContextFor,
  genStOpen,
  pickChainContext,
 )
import Test.Hydra.Ledger.Cardano.Fixtures (slotLength, systemStart)
import Test.Hydra.Prelude (generateWith)
import Test.Hydra.Tx.Gen (
  genConfirmedSnapshot,
  genUTxOAdaOnlyOfSize,
  genValidityBoundsFromContestationPeriod,
 )
import Test.QuickCheck (Gen)

-- | UTxO sizes to dump:
--   28 = new memory ceiling after compiler upgrade (cardano-api 10.18 → 10.21)
--   47 = old ceiling with previous compiler
criticalSizes :: [Int]
criticalSizes = [28, 47]

numberOfParties :: Int
numberOfParties = 10

main :: IO ()
main = do
  putTextLn "Dumping fanout script arguments for Plutus evaluation..."
  putTextLn $ "  Parties:    " <> show numberOfParties
  putTextLn $ "  UTxO sizes: " <> show criticalSizes
  putTextLn ""
  forM_ criticalSizes $ \n -> do
    let seed = n * 1000
    let (tx, knownUTxO, headDatum, redeemer) = generateWith (genFanoutArgs numberOfParties n) seed

    let prefix = "fanout-N" <> show n

    -- Transaction in cardano-cli text envelope format
    let txFile = prefix <> "-tx.json"
    BL.writeFile txFile $ Aeson.encode $ serialiseToTextEnvelope Nothing tx
    putTextLn $ "  Wrote " <> toText txFile

    -- Reference UTxO as JSON (needed to reconstruct ScriptContext)
    let utxoFile = prefix <> "-utxo.json"
    BL.writeFile utxoFile $ Aeson.encode knownUTxO
    putTextLn $ "  Wrote " <> toText utxoFile

    -- ClosedDatum as PlutusData (detailed cardano-cli JSON schema)
    let datumFile = prefix <> "-datum.json"
    BL.writeFile datumFile $
      Aeson.encode $
        scriptDataToJson ScriptDataJsonDetailedSchema headDatum
    putTextLn $ "  Wrote " <> toText datumFile

    -- Fanout redeemer as PlutusData (detailed cardano-cli JSON schema)
    let redeemerFile = prefix <> "-redeemer.json"
    BL.writeFile redeemerFile $
      Aeson.encode $
        scriptDataToJson ScriptDataJsonDetailedSchema $
          unsafeHashableScriptData $
            fromPlutusData $
              toData redeemer
    putTextLn $ "  Wrote " <> toText redeemerFile

    putTextLn ""

  putTextLn "Done."
  putTextLn ""
  putTextLn "The tx + utxo files contain all information needed to reconstruct"
  putTextLn "the ScriptContext via cardano-ledger's evaluateTransactionExecutionUnits."
  putTextLn "Substitute vHead.plutus in the tx witness set with vHead-old.uplc"
  putTextLn "to compare execution budgets across compiler versions 1.45 → 1.56."

-- | Generate a fanout transaction with @numParties@ parties and @numOutputs@
-- UTxO entries. Uses empty commit and decommit UTxOs to isolate the cost of
-- the @hashTxOuts@ loop on fanout outputs only.
genFanoutArgs ::
  Int ->
  Int ->
  Gen (Tx, UTxO, HashableScriptData, Head.Input)
genFanoutArgs numParties numOutputs = do
  utxo <- genUTxOAdaOnlyOfSize numOutputs
  ctx <- genHydraContextFor numParties
  (_committed, stOpen@OpenState{headId, seedTxIn}) <- genStOpen ctx
  let cp = ctxContestationPeriod ctx
  (startSlot, closePoint) <- genValidityBoundsFromContestationPeriod cp
  snapshot <- genConfirmedSnapshot headId 0 1 utxo mempty mempty []
  cctx <- pickChainContext ctx
  let closeTx = unsafeClose cctx (getKnownUTxO stOpen) headId (ctxHeadParameters ctx) 0 snapshot startSlot closePoint
      (_, stClosed) = fromJust $ observeClose stOpen closeTx
      ClosedState{contestationDeadline = deadline} = stClosed
      deadlineSlotNo = slotNoFromUTCTime systemStart slotLength deadline
      lookupUTxO = getKnownUTxO stClosed <> getKnownUTxO cctx
      fanoutTx = unsafeFanout cctx lookupUTxO seedTxIn utxo mempty mempty deadlineSlotNo
      -- The closedUTxO contains exactly the head output with the inline ClosedDatum
      (_, headOut) = fromJust . listToMaybe $ UTxO.toList (getKnownUTxO stClosed)
      headDatum = fromJust $ txOutScriptData (fromCtxUTxOTxOut headOut)
      redeemer =
        Head.Fanout
          { numberOfFanoutOutputs = fromIntegral numOutputs
          , numberOfCommitOutputs = 0
          , numberOfDecommitOutputs = 0
          }
  pure (fanoutTx, lookupUTxO, headDatum, redeemer)
