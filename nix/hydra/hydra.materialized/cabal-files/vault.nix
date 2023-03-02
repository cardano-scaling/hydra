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
    flags = { useghc = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "vault"; version = "0.3.1.5"; };
      license = "BSD-3-Clause";
      copyright = "(c) Heinrich Apfelmus 2011-2013";
      maintainer = "Heinrich Apfelmus <apfelmus at quantentunnel de>";
      author = "Heinrich Apfelmus, Elliott Hird";
      homepage = "https://github.com/HeinrichApfelmus/vault";
      url = "";
      synopsis = "a persistent store for values of arbitrary types";
      description = "A /vault/ is a persistent store for values of arbitrary types.\nIt's like having first-class access to the storage space behind IORefs.\n\nThe data structure is analogous to a bank vault,\nwhere you can access different bank boxes with different keys;\nhence the name.\n\nAlso provided is a /locker/ type, representing a store for a single element.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.0") (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/vault-0.3.1.5.tar.gz";
      sha256 = "ac2a6b6adf58598c5c8faa931ae961a8a2aa50ddb2f0f7a2044ff6e8c3d433a0";
      });
    }) // {
    package-description-override = "Name:               vault\r\nVersion:            0.3.1.5\r\nx-revision: 2\r\nSynopsis:           a persistent store for values of arbitrary types\r\nDescription:\r\n  A /vault/ is a persistent store for values of arbitrary types.\r\n  It's like having first-class access to the storage space behind IORefs.\r\n  .\r\n  The data structure is analogous to a bank vault,\r\n  where you can access different bank boxes with different keys;\r\n  hence the name.\r\n  .\r\n  Also provided is a /locker/ type, representing a store for a single element.\r\n\r\nCategory:           Data\r\nLicense:            BSD3\r\nLicense-file:       LICENSE\r\nAuthor:             Heinrich Apfelmus, Elliott Hird\r\nMaintainer:         Heinrich Apfelmus <apfelmus at quantentunnel de>\r\nHomepage:           https://github.com/HeinrichApfelmus/vault\r\nCopyright:          (c) Heinrich Apfelmus 2011-2013\r\n\r\nbuild-type:         Simple\r\ncabal-version:      >= 1.10\r\nTested-With:         GHC == 7.6.3\r\n                    ,GHC == 7.8.4\r\n                    ,GHC == 7.10.3\r\n                    ,GHC == 8.0.2\r\n                    ,GHC == 8.2.2\r\n                    ,GHC == 8.4.4\r\n                    ,GHC == 8.6.5\r\n                    ,GHC == 8.8.3\r\n                    ,GHC == 8.10.1\r\n\r\nextra-source-files:\r\n    CHANGELOG.md\r\n    README.md\r\n    src/Data/Vault/IO.h\r\n    src/Data/Vault/ST/ST.h\r\n    src/Data/Vault/ST/backends/GHC.h\r\n\r\nsource-repository head\r\n    type:           git\r\n    location:       git://github.com/HeinrichApfelmus/vault.git\r\n\r\nflag UseGHC\r\n    description: Use GHC-specific packages and extensions.\r\n    default:     True\r\n\r\nLibrary\r\n    hs-source-dirs:     src\r\n    build-depends:      base >= 4.5 && < 4.18,\r\n                        containers >= 0.4 && < 0.7,\r\n                        unordered-containers >= 0.2.3.0 && < 0.3,\r\n                        hashable >= 1.1.2.5 && < 1.5\r\n\r\n    if impl(ghc < 8.0)\r\n        build-depends:  semigroups >= 0.1 && < 1.0\r\n\r\n    default-language:   Haskell2010\r\n    default-extensions: CPP\r\n    ghc-options:        -Wall -fno-warn-missing-signatures\r\n\r\n    exposed-modules:\r\n                        Data.Vault.Lazy,\r\n                        Data.Vault.Strict,\r\n                        Data.Vault.ST.Lazy,\r\n                        Data.Vault.ST.Strict,\r\n                        Data.Unique.Really\r\n\r\n    if impl(ghc) && flag(UseGHC)\r\n        CPP-options:    -DUseGHC\r\n";
    }