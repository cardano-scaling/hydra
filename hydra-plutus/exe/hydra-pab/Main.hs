{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Hydra.Prelude

import Control.Monad.Freer (interpret)
import Data.Default (def)
import Hydra.Contract.PAB (PABContract (..), pabPort)
import Plutus.PAB.Effects.Contract.Builtin (Builtin)
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import Plutus.PAB.Simulator (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator as Simulator
import Plutus.PAB.Types (WebserverConfig (..), defaultWebServerConfig)
import qualified Plutus.PAB.Webserver.Server as PAB.Server
import Servant.Client (BaseUrl (..), Scheme (Http))

main :: IO ()
main = void $
  Simulator.runSimulationWith handlers $ do
    shutdown <-
      PAB.Server.startServerDebug' $
        defaultWebServerConfig
          { baseUrl = BaseUrl Http "localhost" pabPort ""
          }

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
  Simulator.mkSimulatorHandlers def def $
    interpret (Builtin.contractHandler Builtin.handleBuiltin)
