module Hydra.TUI.Config where

import Hydra.Prelude

import Data.Text qualified as T
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, doesFileExist, getXdgDirectory)
import System.FilePath (takeDirectory, (</>))

data Theme = DarkTheme | LightTheme
  deriving stock (Eq, Show)

newtype TuiConfig = TuiConfig
  { theme :: Theme
  }

defaultConfig :: TuiConfig
defaultConfig = TuiConfig{theme = DarkTheme}

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
  exists <- doesFileExist path
  if exists
    then parseConfig . decodeUtf8 <$> readFileBS path
    else pure defaultConfig

-- | Persist the config to disk, creating the directory if necessary.
writeConfig :: TuiConfig -> IO ()
writeConfig cfg = do
  path <- configFilePath
  createDirectoryIfMissing True (takeDirectory path)
  writeFileText path (encodeConfig cfg)

-- ---------------------------------------------------------------------------
-- Internal YAML helpers (single-field file — no library needed)
-- ---------------------------------------------------------------------------

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

encodeConfig :: TuiConfig -> Text
encodeConfig TuiConfig{theme} =
  "theme: " <> themeToText theme <> "\n"
 where
  themeToText :: Theme -> Text
  themeToText DarkTheme = "dark"
  themeToText LightTheme = "light"
