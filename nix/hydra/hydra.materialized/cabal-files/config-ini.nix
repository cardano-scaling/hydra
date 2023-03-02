{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  ({
    flags = { enable-doctests = false; };
    package = {
      specVersion = "1.18";
      identifier = { name = "config-ini"; version = "0.2.5.0"; };
      license = "BSD-3-Clause";
      copyright = "©2018 Getty Ritter";
      maintainer = "Getty Ritter <config-ini@infinitenegativeutility.com>";
      author = "Getty Ritter <config-ini@infinitenegativeutility.com>";
      homepage = "https://github.com/aisamanra/config-ini";
      url = "";
      synopsis = "A library for simple INI-based configuration files.";
      description = "The @config-ini@ library is a set of small monadic languages\nfor writing simple configuration languages with convenient,\nhuman-readable error messages.\n\n> parseConfig :: IniParser (Text, Int, Bool)\n> parseConfig = section \"NETWORK\" $ do\n>   user <- field        \"user\"\n>   port <- fieldOf      \"port\" number\n>   enc  <- fieldFlagDef \"encryption\" True\n>   return (user, port, enc)";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
          ];
        buildable = true;
        };
      tests = {
        "test-ini-compat" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."ini" or (errorHandler.buildDepError "ini"))
            (hsPkgs."config-ini" or (errorHandler.buildDepError "config-ini"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          };
        "test-prewritten" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."config-ini" or (errorHandler.buildDepError "config-ini"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            ];
          buildable = true;
          };
        "test-doctest" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = if compiler.isGhc && (compiler.version).lt "7.10" || !flags.enable-doctests
            then false
            else true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/config-ini-0.2.5.0.tar.gz";
      sha256 = "6f7ce53c22392b5b41e3c0a9853e4183c3bbcb18010f9630f48a90f69bbf6f1f";
      });
    }) // {
    package-description-override = "name:             config-ini\nversion:          0.2.5.0\nsynopsis:         A library for simple INI-based configuration files.\nhomepage:         https://github.com/aisamanra/config-ini\nbug-reports:      https://github.com/aisamanra/config-ini/issues\ndescription:      The @config-ini@ library is a set of small monadic languages\n                  for writing simple configuration languages with convenient,\n                  human-readable error messages.\n                  .\n                  > parseConfig :: IniParser (Text, Int, Bool)\n                  > parseConfig = section \"NETWORK\" $ do\n                  >   user <- field        \"user\"\n                  >   port <- fieldOf      \"port\" number\n                  >   enc  <- fieldFlagDef \"encryption\" True\n                  >   return (user, port, enc)\n\nlicense:          BSD3\nlicense-file:     LICENSE\nauthor:           Getty Ritter <config-ini@infinitenegativeutility.com>\nmaintainer:       Getty Ritter <config-ini@infinitenegativeutility.com>\ncopyright:        ©2018 Getty Ritter\ncategory:         Configuration\nbuild-type:       Simple\ncabal-version:    1.18\ntested-with:      GHC == 8.8.4, GHC == 8.10.7, GHC == 9.0.2, GHC == 9.2.4, GHC == 9.4.2\nextra-source-files:\n  README.md,\n  CHANGELOG.md,\n  test/prewritten/cases/*.hs,\n  test/prewritten/cases/*.ini\n\nsource-repository head\n  type: git\n  location: git://github.com/aisamanra/config-ini.git\n\nflag enable-doctests\n  description: Build doctest modules as well (can be finicky)\n  default:     False\n\nlibrary\n  hs-source-dirs:      src\n  exposed-modules:     Data.Ini.Config\n                     , Data.Ini.Config.Bidir\n                     , Data.Ini.Config.Raw\n  ghc-options:         -Wall\n  if impl(ghc > 8.0)\n    ghc-options:       -fno-warn-redundant-constraints\n  build-depends:       base                  >=4.8   && <5\n                     , containers            >=0.5   && <0.7\n                     , text                  >=1.2.2 && <2.1\n                     , unordered-containers  >=0.2.7 && <0.3\n                     , transformers          >=0.4.1 && <0.6\n                     , megaparsec            >=7     && <10\n  default-language:    Haskell2010\n\ntest-suite test-ini-compat\n  type:             exitcode-stdio-1.0\n  ghc-options:      -Wall -threaded\n  default-language: Haskell2010\n  hs-source-dirs:   test/ini-compat\n  main-is:          Main.hs\n  build-depends:    base\n                  , ini >=0.4\n                  , config-ini\n                  , hedgehog\n                  , containers\n                  , unordered-containers\n                  , text\n\ntest-suite test-prewritten\n  type:             exitcode-stdio-1.0\n  ghc-options:      -Wall\n  default-language: Haskell2010\n  hs-source-dirs:   test/prewritten\n  main-is:          Main.hs\n  build-depends:    base\n                  , config-ini\n                  , containers\n                  , unordered-containers\n                  , text\n                  , directory\n\ntest-suite test-doctest\n  if impl(ghc < 7.10) || !flag(enable-doctests)\n    buildable:      False\n  type:             exitcode-stdio-1.0\n  ghc-options:      -Wall\n  default-language: Haskell2010\n  hs-source-dirs:   test/doctest\n  main-is:          Main.hs\n  build-depends:    base\n                  , doctest\n                  , microlens\n                  , text\n";
    }