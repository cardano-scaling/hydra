-- |  'Hydra.API.Projection' module exposes the handle which is our implementation of
--    projections from the CQRS terminology.
--
--    Projections allow us to easily tailor the needs of different API clients
--    (related to our 'ServerOutput' messages) and enable us to more easily implement
--    future user needs.
--
--    This module provides abstract interface for serving different data from the API endpoints
--    and abstracts over the internal implementation (in this case a 'TVar').
--
--    What we serve from the API server is 'Hydra.API.ServerOutput.TimedServerOutputs' and 'Projection' allows us to
--    transform these outputs and add more (stateful) information (like the 'Hydra.API.ServerOutput.HeadStatus' model).
--
--    'Projection's always need to use a function in form of `(model -> event -> model)` where
--    depending on event we are currently dealing with we might want to alter our existing model.
module Hydra.API.Projection where

import Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (MonadLabelledSTM, modifyTVar')

-- | 'Projection' type used to alter/project the API output to suit the client needs.
data Projection stm event model = Projection
  { getLatest :: stm model
  , update :: event -> stm ()
  }

-- | Create a 'Projection' handle that knows how to:
--
-- * get the latest model state
--
-- * update the model using a projection function
mkProjection ::
  MonadLabelledSTM m =>
  String ->
  model ->
  -- | Projection function
  (model -> event -> model) ->
  m (Projection (STM m) event model)
mkProjection lbl startingModel project = do
  tv <- newLabelledTVarIO ("api-server-projection-" <> lbl) startingModel
  pure
    Projection
      { getLatest = readTVar tv
      , update = update tv
      }
 where
  update tv event =
    modifyTVar' tv $ \m ->
      project m event
