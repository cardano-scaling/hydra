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
    flags = { process = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "optparse-applicative"; version = "0.17.0.0"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2012-2017 Paolo Capriotti <paolo@capriotti.io>";
      maintainer = "huw.campbell@gmail.com";
      author = "Paolo Capriotti, Huw Campbell";
      homepage = "https://github.com/pcapriotti/optparse-applicative";
      url = "";
      synopsis = "Utilities and combinators for parsing command line options";
      description = "optparse-applicative is a haskell library for parsing options\non the command line, and providing a powerful applicative\ninterface for composing them.\n\noptparse-applicative takes care of reading and validating the\narguments passed to the command line, handling and reporting\nerrors, generating a usage line, a comprehensive help screen,\nand enabling context-sensitive bash, zsh, and fish completions.\n\nSee the included README for detailed instructions and examples,\nwhich is also available on github\n<https://github.com/pcapriotti/optparse-applicative>.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          (hsPkgs."ansi-wl-pprint" or (errorHandler.buildDepError "ansi-wl-pprint"))
          ] ++ (pkgs.lib).optional (flags.process) (hsPkgs."process" or (errorHandler.buildDepError "process"))) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8")) [
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."fail" or (errorHandler.buildDepError "fail"))
          ];
        buildable = true;
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/optparse-applicative-0.17.0.0.tar.gz";
      sha256 = "825b2e4d3dafe0ba64a073366a88062b3712b81f851793d9ce2327bee70af724";
      });
    }) // {
    package-description-override = "name:                optparse-applicative\nversion:             0.17.0.0\nsynopsis:            Utilities and combinators for parsing command line options\ndescription:\n    optparse-applicative is a haskell library for parsing options\n    on the command line, and providing a powerful applicative\n    interface for composing them.\n    .\n    optparse-applicative takes care of reading and validating the\n    arguments passed to the command line, handling and reporting\n    errors, generating a usage line, a comprehensive help screen,\n    and enabling context-sensitive bash, zsh, and fish completions.\n    .\n    See the included README for detailed instructions and examples,\n    which is also available on github\n    <https://github.com/pcapriotti/optparse-applicative>.\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              Paolo Capriotti, Huw Campbell\nmaintainer:          huw.campbell@gmail.com\ncopyright:           (c) 2012-2017 Paolo Capriotti <paolo@capriotti.io>\ncategory:            System, CLI, Options, Parsing\nbuild-type:          Simple\ncabal-version:       >= 1.10\nextra-source-files:  CHANGELOG.md\n                     README.md\n                     tests/alt.err.txt\n                     tests/cabal.err.txt\n                     tests/carry.err.txt\n                     tests/commands.err.txt\n                     tests/commands_header.err.txt\n                     tests/commands_header_full.err.txt\n                     tests/dropback.err.txt\n                     tests/hello.err.txt\n                     tests/helponempty.err.txt\n                     tests/helponemptysub.err.txt\n                     tests/long_equals.err.txt\n                     tests/formatting.err.txt\n                     tests/formatting-long-subcommand.err.txt\n                     tests/nested.err.txt\n                     tests/optional.err.txt\n                     tests/nested_optional.err.txt\n                     tests/subparsers.err.txt\n\nhomepage:            https://github.com/pcapriotti/optparse-applicative\nbug-reports:         https://github.com/pcapriotti/optparse-applicative/issues\ntested-with:\n  GHC==7.0.4,\n  GHC==7.2.2,\n  GHC==7.4.2,\n  GHC==7.6.3,\n  GHC==7.8.4,\n  GHC==7.10.3,\n  GHC==8.0.2,\n  GHC==8.2.2,\n  GHC==8.4.4,\n  GHC==8.6.5,\n  GHC==8.8.4,\n  GHC==8.10.4,\n  GHC==9.0.1\n\nsource-repository head\n  type:     git\n  location: https://github.com/pcapriotti/optparse-applicative.git\n\nflag process\n  description:\n    Depend on the process package for Bash autocompletion\n  default: True\n\nlibrary\n  hs-source-dirs:      src\n  ghc-options:         -Wall\n  default-language:    Haskell98\n\n  -- See https://ghc.haskell.org/trac/ghc/wiki/Migration/8.0#base-4.9.0.0\n  if impl(ghc >= 8.0)\n    ghc-options:  -Wno-redundant-constraints -Wcompat -Wnoncanonical-monad-instances\n    if impl(ghc < 8.8)\n      ghc-options: -Wnoncanonical-monadfail-instances\n\n  exposed-modules:     Options.Applicative\n                     , Options.Applicative.Arrows\n                     , Options.Applicative.BashCompletion\n                     , Options.Applicative.Builder\n                     , Options.Applicative.Builder.Completer\n                     , Options.Applicative.Builder.Internal\n                     , Options.Applicative.Common\n                     , Options.Applicative.Extra\n                     , Options.Applicative.Help\n                     , Options.Applicative.Help.Chunk\n                     , Options.Applicative.Help.Core\n                     , Options.Applicative.Help.Levenshtein\n                     , Options.Applicative.Help.Pretty\n                     , Options.Applicative.Help.Types\n                     , Options.Applicative.NonEmpty\n                     , Options.Applicative.Types\n                     , Options.Applicative.Internal\n\n  build-depends:       base                            == 4.*\n                     , transformers                    >= 0.2 && < 0.7\n                     , transformers-compat             >= 0.3 && < 0.8\n                     , ansi-wl-pprint                  >= 0.6.8 && < 0.7\n\n  if flag(process)\n    build-depends:     process                         >= 1.0 && < 1.7\n\n  if !impl(ghc >= 8)\n    build-depends:     semigroups                      >= 0.10 && < 0.21\n                     , fail                            == 4.9.*\n\ntest-suite tests\n  type:                exitcode-stdio-1.0\n\n  main-is:             test.hs\n\n  ghc-options:         -Wall -threaded -O2 -funbox-strict-fields\n\n  hs-source-dirs:      tests\n\n  default-language:    Haskell98\n\n  other-modules:       Examples.Alternatives\n                     , Examples.Cabal\n                     , Examples.Commands\n                     , Examples.Formatting\n                     , Examples.Hello\n                     , Examples.LongSub\n\n  build-depends:       base\n                     , optparse-applicative\n                     , QuickCheck                      >= 2.8 && < 2.15\n\n  if !impl(ghc >= 8)\n    build-depends:     semigroups\n";
    }