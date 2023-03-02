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
    flags = { all_cpolys = false; };
    package = {
      specVersion = "1.6";
      identifier = { name = "crypto-api"; version = "0.13.3"; };
      license = "BSD-3-Clause";
      copyright = "Thomas DuBuisson <thomas.dubuisson@gmail.com>";
      maintainer = "Thomas DuBuisson <thomas.dubuisson@gmail.com>";
      author = "Thomas DuBuisson <thomas.dubuisson@gmail.com>, Francisco Blas Izquierdo Riera (klondike)";
      homepage = "https://github.com/TomMD/crypto-api";
      url = "";
      synopsis = "A generic interface for cryptographic operations";
      description = "A generic interface for cryptographic operations (hashes, ciphers, randomness).\nMaintainers of hash and cipher implementations are\nencouraged to add instances for the classes defined\nin Crypto.Classes.  Crypto users are similarly\nencouraged to use the interfaces defined in the Classes\nmodule.\nAny concepts or functions of general use to more than\none cryptographic algorithm (ex: padding) is within\nscope of this package.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cereal" or (errorHandler.buildDepError "cereal"))
          (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          (hsPkgs."entropy" or (errorHandler.buildDepError "entropy"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ] ++ (pkgs.lib).optional (flags.all_cpolys) (hsPkgs."array" or (errorHandler.buildDepError "array"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/crypto-api-0.13.3.tar.gz";
      sha256 = "298a9ea7ce97c8ccf4bfe46d4864092c3a007a56bede73560070db3bf1ac7aa5";
      });
    }) // {
    package-description-override = "name:           crypto-api\r\nversion:        0.13.3\r\nx-revision: 1\r\nlicense:        BSD3\r\nlicense-file:   LICENSE\r\ncopyright:      Thomas DuBuisson <thomas.dubuisson@gmail.com>\r\nauthor:         Thomas DuBuisson <thomas.dubuisson@gmail.com>, Francisco Blas Izquierdo Riera (klondike)\r\nmaintainer:     Thomas DuBuisson <thomas.dubuisson@gmail.com>\r\ndescription:    A generic interface for cryptographic operations (hashes, ciphers, randomness).\r\n\r\n                Maintainers of hash and cipher implementations are\r\n                encouraged to add instances for the classes defined\r\n                in Crypto.Classes.  Crypto users are similarly\r\n                encouraged to use the interfaces defined in the Classes\r\n                module.\r\n\r\n                Any concepts or functions of general use to more than\r\n                one cryptographic algorithm (ex: padding) is within\r\n                scope of this package.\r\nsynopsis:       A generic interface for cryptographic operations\r\ncategory:       Data, Cryptography\r\nhomepage:       https://github.com/TomMD/crypto-api\r\nbug-reports:    https://github.com/TomMD/crypto-api\r\nstability:      stable\r\nbuild-type:     Simple\r\ncabal-version:  >= 1.6\r\ntested-with:    GHC == 7.6.2\r\nextra-source-files:\r\n  cbits/misc.c\r\n  cbits/misc.h\r\n\r\nFlag ALL_CPOLYS\r\n  Description: Build all the CMAC polynomes up to 10000 bits instead of just the usual ones\r\n  Default:     False\r\n  Manual:      True\r\n\r\nLibrary\r\n  -- see https://github.com/TomMD/crypto-api/issues/29\r\n  build-depends: base >= 4.9\r\n\r\n  Build-Depends: base == 4.*,\r\n                 bytestring >= 0.9,\r\n                 cereal >= 0.2,\r\n                 tagged >= 0.1,\r\n                 entropy, transformers\r\n  ghc-options:\r\n  hs-source-dirs:\r\n  exposed-modules: Crypto.Classes, Crypto.Types, Crypto.HMAC,\r\n                   Crypto.Random, Crypto.Padding, Crypto.Modes,\r\n                   Crypto.Util, Crypto.Classes.Exceptions\r\n  other-modules: Crypto.CPoly\r\n  extensions: ForeignFunctionInterface, MultiParamTypeClasses,\r\n              BangPatterns, FunctionalDependencies, FlexibleInstances,\r\n              TypeSynonymInstances\r\n  c-sources: cbits/misc.c\r\n  include-dirs: cbits\r\n  if flag(ALL_CPOLYS)\r\n    Build-Depends: array\r\n    cpp-options: -DALL_CPOLYS\r\n\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: https://github.com/TomMD/crypto-api\r\n";
    }