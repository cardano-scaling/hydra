module Hydra.TUI.Logging.Handlers where

import Brick (EventM)
import Hydra.Prelude
import Hydra.TUI.Logging.Types (LogMessage (..), Severity (..))
import Lens.Micro.Mtl ((%=))

report :: Severity -> UTCTime -> Text -> EventM n [LogMessage] ()
report sev time msg = id %= (LogMessage{severity = sev, message = msg, time, rawJson = Nothing} :)

reportJson :: Severity -> UTCTime -> Text -> Text -> EventM n [LogMessage] ()
reportJson sev time msg json = id %= (LogMessage{severity = sev, message = msg, time, rawJson = Just json} :)

info :: UTCTime -> Text -> EventM n [LogMessage] ()
info = report Info

infoJson :: UTCTime -> Text -> Text -> EventM n [LogMessage] ()
infoJson = reportJson Info

warn :: UTCTime -> Text -> EventM n [LogMessage] ()
warn = report Error
