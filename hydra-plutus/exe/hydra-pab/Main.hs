{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Hydra.Prelude

import Control.Monad.Freer (interpret)
import Data.Default (def)
import Hydra.Contract.PAB (PABContract (..))
import Plutus.PAB.Effects.Contract.Builtin (Builtin)
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import Plutus.PAB.Simulator (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator as Simulator
import qualified Plutus.PAB.Webserver.Server as PAB.Server

main :: IO ()
main = void $
  Simulator.runSimulationWith handlers $ do
    log "Starting plutus-starter PAB webserver on port 8080. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug

    -- Pressing enter results in the balances being printed
    void $ liftIO getLine

    log "Balances at the end of the simulation"
    b <- Simulator.currentBalances
    Simulator.logBalances @(Builtin PABContract) b

    shutdown
 where
  log = Simulator.logString @(Builtin PABContract)

handlers :: SimulatorEffectHandlers (Builtin PABContract)
handlers =
  Simulator.mkSimulatorHandlers @(Builtin PABContract) def $
    interpret (Builtin.contractHandler Builtin.handleBuiltin)
