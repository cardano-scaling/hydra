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
    flags = { lib-werror = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "katip"; version = "0.8.7.2"; };
      license = "BSD-3-Clause";
      copyright = "Soostone Inc, 2015-2017";
      maintainer = "michael.xavier@soostone.com";
      author = "Ozgun Ataman, Michael Xavier";
      homepage = "https://github.com/Soostone/katip";
      url = "";
      synopsis = "A structured logging framework.";
      description = "Katip is a structured logging framework. See README.md for more details.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."auto-update" or (errorHandler.buildDepError "auto-update"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."either" or (errorHandler.buildDepError "either"))
          (hsPkgs."safe-exceptions" or (errorHandler.buildDepError "safe-exceptions"))
          (hsPkgs."hostname" or (errorHandler.buildDepError "hostname"))
          (hsPkgs."old-locale" or (errorHandler.buildDepError "old-locale"))
          (hsPkgs."string-conv" or (errorHandler.buildDepError "string-conv"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
          (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          (hsPkgs."microlens-th" or (errorHandler.buildDepError "microlens-th"))
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."unliftio-core" or (errorHandler.buildDepError "unliftio-core"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          ] ++ (if system.isWindows
          then [ (hsPkgs."Win32" or (errorHandler.buildDepError "Win32")) ]
          else [ (hsPkgs."unix" or (errorHandler.buildDepError "unix")) ]);
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."katip" or (errorHandler.buildDepError "katip"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-golden" or (errorHandler.buildDepError "tasty-golden"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."time-locale-compat" or (errorHandler.buildDepError "time-locale-compat"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."regex-tdfa" or (errorHandler.buildDepError "regex-tdfa"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."safe-exceptions" or (errorHandler.buildDepError "safe-exceptions"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."blaze-builder" or (errorHandler.buildDepError "blaze-builder"))
            (hsPkgs."katip" or (errorHandler.buildDepError "katip"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."safe-exceptions" or (errorHandler.buildDepError "safe-exceptions"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/katip-0.8.7.2.tar.gz";
      sha256 = "d990751023c8dbfaa3626bb237ffecf9b7c76f7938c14b620ac2e39aefab070a";
      });
    }) // {
    package-description-override = "name:                katip\nversion:             0.8.7.2\nsynopsis:            A structured logging framework.\ndescription:\n  Katip is a structured logging framework. See README.md for more details.\n\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              Ozgun Ataman, Michael Xavier\nmaintainer:          michael.xavier@soostone.com\ncopyright:           Soostone Inc, 2015-2017\ncategory:            Data, Text, Logging\nhomepage:            https://github.com/Soostone/katip\nbug-reports:         https://github.com/Soostone/katip/issues\nbuild-type:          Simple\ncabal-version:       >=1.10\nextra-source-files:\n  README.md\n  changelog.md\n  examples/example.hs\n  examples/example_lens.hs\n  bench/Main.hs\n  test/Main.hs\n  test/Katip/Tests.hs\n  test/Katip/Tests/Scribes/Handle.hs\n  test/Katip/Tests/Scribes/Handle-text.golden\n  test/Katip/Tests/Format/Time.hs\ntested-with: GHC == 8.0.2, GHC == 8.2.2, GHC == 8.4.3, GHC == 8.6.5, GHC == 8.8.4, GHC == 8.10.7, GHC == 9.0.2\n\nsource-repository head\n  type:     git\n  location: https://github.com/Soostone/katip.git\n\nflag lib-Werror\n  default: False\n  manual: True\n\nlibrary\n  exposed-modules:\n    Katip\n    Katip.Core\n    Katip.Format.Time\n    Katip.Monadic\n    Katip.Scribes.Handle\n\n  default-extensions:\n    DeriveGeneric\n    FlexibleContexts\n    GeneralizedNewtypeDeriving\n    MultiParamTypeClasses\n    RankNTypes\n    RecordWildCards\n    TemplateHaskell\n    OverloadedStrings\n\n  build-depends: base >=4.9.0.0 && <5\n               , aeson >=1.0.0.0\n               , async >= 2.0.0.0 && < 3.0.0.0\n               , auto-update >= 0.1\n               , bytestring >= 0.9\n               , containers >=0.4\n               , either >= 4\n               , safe-exceptions >= 0.1.0.0\n               , hostname >=1.0\n               , old-locale >= 1.0\n               , string-conv >= 0.1\n               , template-haskell >= 2.8\n               , text >= 1.2.4.0\n               , time >= 1\n               , transformers >= 0.3\n               , transformers-compat\n               , unordered-containers >= 0.2\n               , monad-control >= 1.0\n               , mtl >= 2.0\n               , transformers-base >= 0.3\n               , resourcet >= 1.2.0\n               , scientific >= 0.3.3.0\n               , microlens >= 0.2.0.0\n               , microlens-th >= 0.1.0.0\n               , semigroups\n               , unliftio-core >= 0.1\n               , stm >= 2.4.4.1\n\n  hs-source-dirs:      src\n  default-language:    Haskell2010\n  ghc-options:        -Wall\n  if flag(lib-Werror)\n    ghc-options: -Werror\n  if os(windows)\n    build-depends: Win32 >=2.3 && <2.9\n    exposed-modules: Katip.Compat\n  else\n    build-depends: unix >= 2.5 && <2.8\n\n\ntest-suite test\n  type: exitcode-stdio-1.0\n  main-is: Main.hs\n  hs-source-dirs: test\n  other-modules:\n    Katip.Tests.Format.Time\n    Katip.Tests.Scribes.Handle\n    Katip.Tests\n  default-language:    Haskell2010\n  build-depends: base\n               , katip\n               , aeson\n               , bytestring\n               , tasty >= 0.10.1.2\n               , tasty-golden\n               , tasty-hunit\n               , tasty-quickcheck\n               , quickcheck-instances\n               , template-haskell\n               , text\n               , time\n               , time-locale-compat >= 0.1.0.1\n               , directory\n               , regex-tdfa\n               , unordered-containers\n               , microlens\n               , containers\n               , stm\n               , safe-exceptions\n  ghc-options: -Wall\n  if flag(lib-Werror)\n    ghc-options: -Werror\n\n\nbenchmark bench\n  type: exitcode-stdio-1.0\n  main-is: Main.hs\n  hs-source-dirs: bench\n  default-language:    Haskell2010\n  ghc-options: -O2 -Wall -threaded -rtsopts -with-rtsopts=-N\n  if flag(lib-Werror)\n    ghc-options: -Werror\n  build-depends:\n                 base\n               , aeson\n               , blaze-builder\n               , katip\n               , criterion >= 1.1.0.0\n               , unix\n               , text\n               , time\n               , transformers\n               , deepseq\n               , async\n               , filepath\n               , safe-exceptions\n               , directory\n";
    }