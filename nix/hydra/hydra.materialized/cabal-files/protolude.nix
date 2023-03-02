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
    flags = { dev = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "protolude"; version = "0.3.0"; };
      license = "MIT";
      copyright = "2016-2020 Stephen Diehl";
      maintainer = "stephen.m.diehl@gmail.com";
      author = "Stephen Diehl";
      homepage = "https://github.com/sdiehl/protolude";
      url = "";
      synopsis = "A small prelude.";
      description = "A sensible set of defaults for writing custom Preludes.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."mtl-compat" or (errorHandler.buildDepError "mtl-compat"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."fail" or (errorHandler.buildDepError "fail"));
        buildable = true;
        };
      exes = {
        "exports" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."ghc" or (errorHandler.buildDepError "ghc"))
            (hsPkgs."ghc-paths" or (errorHandler.buildDepError "ghc-paths"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."protolude" or (errorHandler.buildDepError "protolude"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          buildable = if flags.dev then true else false;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/protolude-0.3.0.tar.gz";
      sha256 = "4083385a9e03fab9201f63ce198b9ced3fbc1c50d6d42574db5e36c757bedcac";
      });
    }) // {
    package-description-override = "name:               protolude\nversion:            0.3.0\nsynopsis:           A small prelude.\ndescription:        A sensible set of defaults for writing custom Preludes.\nhomepage:           https://github.com/sdiehl/protolude\nlicense:            MIT\nlicense-file:       LICENSE\nauthor:             Stephen Diehl\nmaintainer:         stephen.m.diehl@gmail.com\ncopyright:          2016-2020 Stephen Diehl\ncategory:           Prelude\nbuild-type:         Simple\ncabal-version:      >=1.10\nbug-reports:        https://github.com/sdiehl/protolude/issues\ntested-with:\n  GHC ==7.6.1\n   || ==7.6.2\n   || ==7.6.3\n   || ==7.8.1\n   || ==7.8.2\n   || ==7.8.3\n   || ==7.8.4\n   || ==7.10.1\n   || ==7.10.2\n   || ==7.10.3\n   || ==8.0.1\n   || ==8.2.1\n   || ==8.4.1\n   || ==8.6.1\n   || ==8.8.1\n   || ==8.10.1\n\nextra-source-files:\n  README.md\n  ChangeLog.md\n\nflag dev\n  description: Build development tools\n  manual:      True\n  default:     False\n\nsource-repository head\n  type:     git\n  location: git@github.com:protolude/protolude.git\n\nlibrary\n  exposed-modules:\n    Protolude\n    Protolude.Applicative\n    Protolude.Base\n    Protolude.Bifunctor\n    Protolude.Bool\n    Protolude.CallStack\n    Protolude.Conv\n    Protolude.ConvertText\n    Protolude.Debug\n    Protolude.Either\n    Protolude.Error\n    Protolude.Exceptions\n    Protolude.Functor\n    Protolude.List\n    Protolude.Monad\n    Protolude.Panic\n    Protolude.Partial\n    Protolude.Safe\n    Protolude.Semiring\n    Protolude.Show\n    Protolude.Unsafe\n\n  default-extensions:\n    NoImplicitPrelude\n    FlexibleContexts\n    MultiParamTypeClasses\n    OverloadedStrings\n\n  ghc-options:        -Wall -fwarn-implicit-prelude\n  build-depends:\n      array                >=0.4  && <0.6\n    , async                >=2.0  && <2.3\n    , base                 >=4.6  && <4.15\n    , bytestring           >=0.10 && <0.11\n    , containers           >=0.5  && <0.7\n    , deepseq              >=1.3  && <1.5\n    , ghc-prim             >=0.3  && <0.7\n    , hashable             >=1.2  && <1.4\n    , mtl                  >=2.1  && <2.3\n    , mtl-compat           >=0.2  && <0.3\n    , stm                  >=2.4  && <2.6\n    , text                 >=1.2  && <1.3\n    , transformers         >=0.2  && <0.6\n    , transformers-compat  >=0.4  && <0.7\n\n  if !impl(ghc >=8.0)\n    build-depends: fail ==4.9.*\n\n  hs-source-dirs:     src\n  default-language:   Haskell2010\n\nexecutable exports\n  main-is:          Exports.hs\n  default-language: Haskell2010\n\n  if flag(dev)\n    buildable: True\n\n  else\n    buildable: False\n\n  build-depends:\n      base          >=4.6 && <4.15\n    , directory\n    , filepath\n    , ghc\n    , ghc-paths\n    , mtl\n    , process\n    , protolude\n    , transformers\n";
    }