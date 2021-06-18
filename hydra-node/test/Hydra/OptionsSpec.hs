module Hydra.OptionsSpec where

import Hydra.Prelude
import Hydra.Options (Options (..), defaultOptions, getParseResult, parseHydraOptionsFromString)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Hydra Node Options" $ do
  it "parses --host option given valid IPv4 and IPv6 addresses" $ do
    parseOptions ["--host", "127.0.0.1"]
      `shouldBe` Just defaultOptions{host = "127.0.0.1"}
    parseOptions ["--host", "2001:db8:11e:c00::101"]
      `shouldBe` Just defaultOptions{host = "2001:db8:11e:c00::101"}
    parseOptions ["--host", "0.0.0.0"]
      `shouldBe` Just defaultOptions{host = "0.0.0.0"}
    parseOptions ["--host", "0.0.0"]
      `shouldBe` Nothing
    parseOptions ["--host", "2001:db8:11e:c00:101"]
      `shouldBe` Nothing

  it "parses --port option given valid port number" $ do
    parseOptions ["--port", "12345"]
      `shouldBe` Just defaultOptions{port = 12345}
    parseOptions ["--port", "123456"]
      `shouldBe` Nothing
    parseOptions ["--port", "0"]
      `shouldBe` Just defaultOptions{port = 0}
    parseOptions ["--port", "-42"]
      `shouldBe` Nothing

  it "parses --peer <host>@<port> option" $ do
    parseOptions ["--peer", "1.2.3.4@4567"]
      `shouldBe` Just defaultOptions{peers = [("1.2.3.4", 4567)]}
    parseOptions ["--peer", ":::1@4567"]
      `shouldBe` Just defaultOptions{peers = [(":::1", 4567)]}
    parseOptions ["--peer", "1.2.3.4@4567", "--peer", "1.2.3.5@4568"]
      `shouldBe` Just defaultOptions{peers = [("1.2.3.4", 4567), ("1.2.3.5", 4568)]}
    parseOptions ["--peer", "foo.com@4567"]
      `shouldBe` Just defaultOptions{peers = [("foo.com", 4567)]}
    parseOptions ["--peer", "foo.com@456789"]
      `shouldBe` Nothing

  it "parses --monitoring-port option given valid port number" $ do
    parseOptions []
      `shouldBe` Just defaultOptions{monitoringPort = Nothing}
    parseOptions ["--monitoring-port", "12345"]
      `shouldBe` Just defaultOptions{monitoringPort = Just 12345}

  it "parses --version flag as a parse error" $ do
    parseOptions ["--version"] `shouldBe` Nothing

  it "parses --party option as a filepath" $ do
    parseOptions ["--party", "./alice.vk"] `shouldBe` Just defaultOptions{parties = ["./alice.vk"]}
    parseOptions ["--party", "/foo"] `shouldBe` Just defaultOptions{parties = ["/foo"]}
    parseOptions ["--party", "bar"] `shouldBe` Just defaultOptions{parties = ["bar"]}
    parseOptions ["--party", "alice.vk", "--party", "bob.vk"]
      `shouldBe` Just defaultOptions{parties = ["alice.vk", "bob.vk"]}

parseOptions :: [String] -> Maybe Options
parseOptions = getParseResult . parseHydraOptionsFromString
