{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.Test.Utils where

import Hydra.Prelude

import Ledger

import Control.Lens (view, (^.))
import Control.Monad.Freer (Eff, Member)
import Control.Monad.Freer.Writer (tell)
import Data.Maybe (fromJust)
import Data.Text.Prettyprint.Doc (Doc, (<+>))
import Ledger.AddressMap (UtxoMap, fundsAt)
import Plutus.Contract (Contract, HasEndpoint)
import Plutus.Trace.Effects.RunContract (ContractConstraints, RunContract)
import Plutus.Trace.Effects.Waiting (Waiting)
import Plutus.Trace.Emulator.Types (ContractHandle, UserThreadMsg (..), walletInstanceTag)
import PlutusTx (CompiledCode, IsData (..), getPir)
import Prettyprinter (pretty)
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsString)
import Wallet.Emulator.Chain (chainNewestFirst)
import Wallet.Emulator.Folds (postMapM)
import Wallet.Emulator.MultiAgent (EmulatorTimeEvent (..), chainUtxo, emulatorState)
import Wallet.Emulator.Wallet (walletAddress)

import Plutus.Contract.Test (
  TracePredicate,
  Wallet (..),
  assertAccumState,
  walletPubKey,
 )

import qualified Control.Foldl as L
import qualified Data.Map as Map
import qualified Plutus.Trace.Emulator as Trace
import qualified Prettyprinter as Pretty
import qualified Wallet.Emulator.Folds as Folds

--
-- Predicates
--

-- | Check that the given script address has some `DatumHash`.
datumAtAddress :: IsData a => Address -> a -> TracePredicate
datumAtAddress address expected =
  flip postMapM (L.generalize $ Folds.utxoAtAddress address) $ \utxo -> do
    let datums = findAllDatums utxo
        expectedDatum = datumHash $ Datum $ toData expected
    if expectedDatum `elem` datums
      then return True
      else do
        tell @(Doc Void)
          ( "Expected datum with hash"
              <+> pretty expectedDatum
              <+> ", got datums at address"
              <+> pretty address
              <+> ":"
              <+> pretty datums
          )
        return False
 where
  findAllDatums :: UtxoMap -> [DatumHash]
  findAllDatums utxoMap =
    catMaybes $ Map.elems $ Map.map (txOutDatum . txOutTxOut) utxoMap

-- | Assert equality with the last user-defined log line from an emulator
-- execution. This is a poor's man way of yielding some information from a test
-- run to the 'TracePredicate'. Practically:
--
--     scenario :: Trace.EmulatorTrace ()
--     scenario = do
--         ...
--         Trace.logInfo "Patate"
--
--     >>> checkPredicate "myScenario" (assertLastLog "Patate") scenario
--     Ok.
assertLastLog :: String -> TracePredicate
assertLastLog expected = do
  flip postMapM (L.generalize Folds.userLog) $ \logs ->
    case reverse logs of
      (EmulatorTimeEvent _ (UserLog got)) : _ -> return (got == expected)
      _ -> return False

-- | A tiny wrapper around 'assertAccumState' but when the state is a list.
assertFinalState ::
  (ContractConstraints schema, Show st) =>
  Contract [st] schema e a ->
  Wallet ->
  (st -> Bool) ->
  TracePredicate
assertFinalState contract wallet predicate =
  assertAccumState
    contract
    (walletInstanceTag wallet)
    (maybe False (predicate . last) . nonEmpty)
    "assert final state"

checkCompiledContractPIR :: FilePath -> CompiledCode a -> TestTree
checkCompiledContractPIR path code =
  goldenVsString "PIR" path (return $ fromString $ show $ pretty $ fromJust $ getPir code)

--
-- In Emulator
--

-- | Inspect the current UTxO of a wallet in an Emulator scenario.
utxoOf :: Wallet -> Trace.EmulatorTrace UtxoMap
utxoOf w = do
  st <- emulatorState . view chainNewestFirst <$> Trace.chainState
  return $ st ^. chainUtxo . fundsAt (walletAddress w)

-- | Pretty print a 'UtxoMap', for debugging / logging.
prettyUtxo :: UtxoMap -> String
prettyUtxo =
  show
    . Pretty.list
    . fmap (\(outRef, tx) -> Pretty.tupled [pretty outRef, pretty (txOutTxOut tx)])
    . Map.toList

callEndpoint ::
  forall l ep w s e effs.
  ( ContractConstraints s
  , HasEndpoint l ep s
  , Member RunContract effs
  , Member Waiting effs
  ) =>
  ContractHandle w s e ->
  ep ->
  Eff effs ()
callEndpoint hdl v = do
  Trace.callEndpoint @l hdl v
  void Trace.nextSlot

--
-- Miscellaneous
--

-- | Get the public key corresponding to a wallet
vk :: Wallet -> PubKeyHash
vk = pubKeyHash . walletPubKey
