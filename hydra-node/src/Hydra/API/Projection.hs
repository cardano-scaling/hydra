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
newProjection startingModel events project = do
  tv <- atomically $ do
    newTVar startingModel
  -- TODO: mapM_ (project projection projectHeadStatus) events
  pure
    Projection
      { getLatest = readTVar tv
      , update = \event ->
          modifyTVar' tv $ \m ->
            project m event
      }
