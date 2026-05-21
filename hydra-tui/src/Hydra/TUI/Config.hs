module Hydra.TUI.Config where

import Hydra.Prelude

import Control.Exception (IOException)
import Data.Text qualified as T
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, getXdgDirectory)
import System.FilePath (takeDirectory, (</>))

data Theme = DarkTheme | LightTheme
  deriving stock (Eq, Show)

newtype TuiConfig = TuiConfig
  { theme :: Theme
  }
  deriving stock (Eq, Show)

-- | Default config used when no file is present or parsing fails.
defaultConfig :: TuiConfig
defaultConfig = TuiConfig{theme = DarkTheme}

-- | Flip between dark and light themes.
toggleTheme :: Theme -> Theme
toggleTheme DarkTheme = LightTheme
toggleTheme LightTheme = DarkTheme

-- | Path to the persisted config: @$XDG_CONFIG_HOME/hydra/tui-config.yaml@.
configFilePath :: IO FilePath
configFilePath = do
  xdgConfig <- getXdgDirectory XdgConfig "hydra"
  pure $ xdgConfig </> "tui-config.yaml"

-- | Load config from disk, falling back to 'defaultConfig' on missing or
-- unreadable files.
readConfig :: IO TuiConfig
readConfig = do
  path <- configFilePath
  result <- try (readFileBS path)
  pure $ case result of
    Left (_ :: IOException) -> defaultConfig
    Right bytes -> parseConfig (decodeUtf8 bytes)

-- | Persist the config to disk, creating the directory if necessary. Silently
-- swallows IO errors so an unwritable config path never crashes the TUI.
writeConfig :: TuiConfig -> IO ()
writeConfig cfg = do
  path <- configFilePath
  result <- try $ do
    createDirectoryIfMissing True (takeDirectory path)
    writeFileText path (encodeConfig cfg)
  case result of
    Left (_ :: IOException) -> pure ()
    Right () -> pure ()

-- ---------------------------------------------------------------------------
-- Internal YAML helpers
-- ---------------------------------------------------------------------------

-- | Parse the on-disk config text. Unrecognised content falls back to 'defaultConfig'.
parseConfig :: Text -> TuiConfig
parseConfig content =
  TuiConfig{theme = fromMaybe (theme defaultConfig) parsedTheme}
 where
  parsedTheme =
    listToMaybe
      [ themeFromText v
      | line <- T.lines content
      , Just rest <- [T.stripPrefix "theme: " line]
      , let v = T.strip rest
      ]

  themeFromText :: Text -> Theme
  themeFromText "light" = LightTheme
  themeFromText _ = DarkTheme

-- | Serialise the config to its on-disk text form.
encodeConfig :: TuiConfig -> Text
encodeConfig TuiConfig{theme} =
  "theme: " <> themeToText theme <> "\n"
 where
  themeToText :: Theme -> Text
  themeToText DarkTheme = "dark"
  themeToText LightTheme = "light"
