module Hydra.TUI.Handlers.Global where

import Brick (EventM)
import Brick.Main (halt)
import Graphics.Vty (Event (..), Key (..), Modifier (..))
import qualified Graphics.Vty as Vty
import Hydra.Prelude

handleVtyQuitEvents :: Vty.Event -> EventM n s ()
handleVtyQuitEvents = \case
  EvKey (KChar 'c') [MCtrl] -> halt
  EvKey (KChar 'd') [MCtrl] -> halt
  EvKey (KChar 'q') [] -> halt
  _ -> pure ()

handleVtyGlobalEvents :: Vty.Event -> EventM n s ()
handleVtyGlobalEvents e = do
  handleVtyQuitEvents e
