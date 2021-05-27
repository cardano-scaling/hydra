module Hydra.OptionSpec where

import Cardano.Prelude
import Data.String (String)
import Hydra.Option (Option (..), defaultOption, getParseResult, parseHydraOptionsFromString)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Hydra Node Options" $ do
  it "parses --host option given valid IPv4 and IPv6 addresses" $ do
    parseOptions ["--host", "127.0.0.1"]
      `shouldBe` Just defaultOption{host = "127.0.0.1"}
    parseOptions ["--host", "2001:db8:11e:c00::101"]
      `shouldBe` Just defaultOption{host = "2001:db8:11e:c00::101"}
    parseOptions ["--host", "0.0.0.0"]
      `shouldBe` Just defaultOption{host = "0.0.0.0"}
    parseOptions ["--host", "0.0.0"]
      `shouldBe` Nothing
    parseOptions ["--host", "2001:db8:11e:c00:101"]
      `shouldBe` Nothing

  it "parses --port option given valid port number" $ do
    parseOptions ["--port", "12345"]
      `shouldBe` Just defaultOption{port = 12345}
    parseOptions ["--port", "123456"]
      `shouldBe` Nothing
    parseOptions ["--port", "0"]
      `shouldBe` Nothing
    parseOptions ["--port", "-42"]
      `shouldBe` Nothing

  it "parses --peer <host>@<port> option" $ do
    parseOptions ["--peer", "1.2.3.4@4567"]
      `shouldBe` Just defaultOption{peers = [("1.2.3.4", 4567)]}
    parseOptions ["--peer", ":::1@4567"]
      `shouldBe` Just defaultOption{peers = [(":::1", 4567)]}
    parseOptions ["--peer", "1.2.3.4@4567", "--peer", "1.2.3.5@4568"]
      `shouldBe` Just defaultOption{peers = [("1.2.3.4", 4567), ("1.2.3.5", 4568)]}
    parseOptions ["--peer", "foo.com@4567"]
      `shouldBe` Just defaultOption{peers = [("foo.com", 4567)]}
    parseOptions ["--peer", "foo.com@456789"]
      `shouldBe` Nothing

parseOptions :: [String] -> Maybe Hydra.Option.Option
parseOptions = getParseResult . parseHydraOptionsFromString
