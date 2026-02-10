module Hydra.TUI.Logging.Handlers where

import "brick" Brick (EventM)
import "hydra-prelude" Hydra.Prelude
import "hydra-tui" Hydra.TUI.Logging.Types (LogMessage (..), Severity (..))
import "microlens-mtl" Lens.Micro.Mtl ((%=))

report :: Severity -> UTCTime -> Text -> EventM n [LogMessage] ()
report sev time msg = id %= (LogMessage sev msg time :)

info :: UTCTime -> Text -> EventM n [LogMessage] ()
info = report Info

warn :: UTCTime -> Text -> EventM n [LogMessage] ()
warn = report Error
