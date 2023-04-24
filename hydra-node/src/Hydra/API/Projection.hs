module Hydra.API.Projection where

import Hydra.Prelude

import Control.Monad.Class.MonadSTM (modifyTVar', newTVar)

-- | Projection type used to alter/project the API output to suit client needs
data Projection stm event model = Projection
  { getLatest :: stm model
  , update :: event -> stm ()
  }

newProjection ::
  MonadSTM m =>
  model ->
  [event] ->
  -- | Projection function
  (model -> event -> model) ->
  m (Projection (STM m) event model)
newProjection startingModel events project =
  atomically $ do
    tv <- newTVar startingModel
    mapM_ (update tv) events
    pure
      Projection
        { getLatest = readTVar tv
        , update = update tv
        }
 where
  update tv event =
    modifyTVar' tv $ \m ->
      project m event
