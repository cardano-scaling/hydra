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
    flags = {};
    package = {
      specVersion = "1.8";
      identifier = { name = "RSA"; version = "2.4.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Adam Wick <awick@galois.com>";
      author = "Adam Wick <awick@galois.com>";
      homepage = "";
      url = "";
      synopsis = "Implementation of RSA, using the padding schemes of PKCS#1 v2.1.";
      description = "This library implements the RSA encryption and signature\nalgorithms for arbitrarily-sized ByteStrings. While the\nimplementations work, they are not necessarily the fastest ones\non the planet. Particularly key generation. The algorithms\nincluded are based of RFC 3447, or the Public-Key Cryptography\nStandard for RSA, version 2.1 (a.k.a, PKCS#1 v2.1).";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."crypto-api" or (errorHandler.buildDepError "crypto-api"))
          (hsPkgs."crypto-pubkey-types" or (errorHandler.buildDepError "crypto-pubkey-types"))
          (hsPkgs."SHA" or (errorHandler.buildDepError "SHA"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.0") (hsPkgs."cipher-aes128" or (errorHandler.buildDepError "cipher-aes128"));
        buildable = true;
        };
      tests = {
        "test-rsa" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."crypto-api" or (errorHandler.buildDepError "crypto-api"))
            (hsPkgs."crypto-pubkey-types" or (errorHandler.buildDepError "crypto-pubkey-types"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
            (hsPkgs."SHA" or (errorHandler.buildDepError "SHA"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/RSA-2.4.1.tar.gz";
      sha256 = "72c5d8c45ef1013e0e8aff763bb8894df0f022f28e698e33ae87bbdb33d69041";
      });
    }) // {
    package-description-override = "name:       RSA\ncategory:   Cryptography, Codec\nversion:    2.4.1\nlicense:    BSD3\nlicense-file: LICENSE\nauthor:     Adam Wick <awick@galois.com>\nmaintainer: Adam Wick <awick@galois.com>\nstability:  stable\nbuild-type: Simple\ncabal-version: >= 1.8\ntested-with: GHC ==7.8.0\nsynopsis: Implementation of RSA, using the padding schemes of PKCS#1 v2.1.\ndescription: This library implements the RSA encryption and signature \n             algorithms for arbitrarily-sized ByteStrings. While the \n             implementations work, they are not necessarily the fastest ones\n             on the planet. Particularly key generation. The algorithms\n             included are based of RFC 3447, or the Public-Key Cryptography\n             Standard for RSA, version 2.1 (a.k.a, PKCS#1 v2.1).   \n\nLibrary\n  hs-source-dirs:  src\n  build-depends:   base                >= 4.6     && < 7.0,\n                   binary              >  0.7     && < 1.0,\n                   bytestring          >  0.8     && < 0.12,\n                   crypto-api          >= 0.10    && < 0.14,\n                   crypto-pubkey-types >= 0.2     && < 0.6,\n                   SHA                 >= 1.6.4.1 && < 2.0\n  if impl(ghc < 8.0)\n    build-depends: cipher-aes128       < 0.7.0.4\n  exposed-modules: Codec.Crypto.RSA,\n                   Codec.Crypto.RSA.Exceptions,\n                   Codec.Crypto.RSA.Pure\n  GHC-Options:     -Wall -fno-warn-orphans\n  extensions:      DeriveDataTypeable, MultiWayIf\n\ntest-suite test-rsa\n  type:           exitcode-stdio-1.0\n  Main-Is:        Test.hs\n  hs-source-dirs: src,.\n  other-modules:  Codec.Crypto.RSA.Pure\n  build-depends:  base                       >= 4.6     && < 7.0,\n                  binary                     >  0.7     && < 1.0,\n                  bytestring                 >  0.8     && < 0.12,\n                  crypto-api                 >= 0.10    && < 0.14,\n                  crypto-pubkey-types        >= 0.4     && < 0.6,\n                  QuickCheck                 >= 2.5     && < 3,\n                  tagged                     >= 0.2     && < 0.9,\n                  test-framework             >= 0.8.0.3 && < 0.10,\n                  test-framework-quickcheck2 >= 0.3.0.2 && < 0.5,\n                  SHA                        >= 1.6.4.1 && < 2.0\n  GHC-Options:    -Wall -fno-warn-orphans\n  extensions:     DeriveDataTypeable, MultiWayIf, ScopedTypeVariables\n\nsource-repository head\n  type: git\n  location: git://github.com/GaloisInc/RSA.git\n\n";
    }