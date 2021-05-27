module Hydra.OptionSpec where

import Cardano.Prelude
import Data.String (String)
import Hydra.Option (Option (..), defaultOption, getParseResult, parseHydraOptionsFromString)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Hydra Node Options" $ do
  it "parses valid IPv4 and IPv6 addresses for --host option" $ do
    parseOptions ["--node-id", "1", "--host", "127.0.0.1"]
      `shouldBe` Just defaultOption{host = "127.0.0.1"}
    parseOptions ["--node-id", "1", "--host", "2001:db8:11e:c00::101"]
      `shouldBe` Just defaultOption{host = "2001:db8:11e:c00::101"}
    parseOptions ["--node-id", "1", "--host", "0.0.0.0"]
      `shouldBe` Just defaultOption{host = "0.0.0.0"}
    parseOptions ["--node-id", "1", "--host", "0.0.0"]
      `shouldBe` Nothing
    parseOptions ["--node-id", "1", "--host", "2001:db8:11e:c00:101"]
      `shouldBe` Nothing

  it "parses valid port number --port option" $ do
    parseOptions ["--node-id", "1", "--port", "12345"]
      `shouldBe` Just defaultOption{port = 12345}
    parseOptions ["--node-id", "1", "--port", "123456"]
      `shouldBe` Nothing
    parseOptions ["--node-id", "1", "--port", "0"]
      `shouldBe` Nothing
    parseOptions ["--node-id", "1", "--port", "-42"]
      `shouldBe` Nothing

parseOptions :: [String] -> Maybe Hydra.Option.Option
parseOptions = getParseResult . parseHydraOptionsFromString
