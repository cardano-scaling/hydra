-- | Golden and roundtrip tests for the persisted event format ('StateEvent'
-- and its 'StateChanged' payload).
--
-- 'StateChanged' events are what the node persists in its event log (regardless
-- of the storage backend), so these tests guard the on-disk JSON: a change here
-- means older event logs may fail to load and must be treated as a **breaking**
-- change to persistence.
module Hydra.HeadLogic.OutcomeSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

-- IsChainState Tx instance, needed to (de)serialize 'StateEvent'/'StateChanged' at 'Tx'.
import Hydra.Chain.Direct.State ()
import Hydra.HeadLogic (StateChanged)
import Hydra.HeadLogic.StateEvent (StateEvent (..))
import Hydra.Ledger.Cardano (Tx)
import Test.Aeson.GenericSpecs (defaultSettings, roundtripAndGoldenADTSpecsWithSettings, roundtripAndGoldenSpecsWithSettings, sampleSize)
import Test.Hydra.Chain.Direct.State ()
import Test.Hydra.HeadLogic.Outcome ()
import Test.Hydra.HeadLogic.StateEvent ()

spec :: Spec
spec =
  describe "persisted event format" $ do
    -- NOTE: Whenever one of these fails, make sure to record a **BREAKING**
    -- change of the persisted event format (older event logs may then fail to
    -- load). These guard the JSON of every persisted 'StateChanged' variant.
    roundtripAndGoldenSpecsWithSettings (defaultSettings{sampleSize = 1}) (Proxy @(MinimumSized (StateEvent Tx)))
    roundtripAndGoldenADTSpecsWithSettings (defaultSettings{sampleSize = 1}) (Proxy @(MinimumSized (StateChanged Tx)))
