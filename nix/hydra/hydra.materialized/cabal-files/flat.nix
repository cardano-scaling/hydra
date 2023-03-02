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
      specVersion = "1.10";
      identifier = { name = "flat"; version = "0.4.4.0.0.0.0.2"; };
      license = "BSD-3-Clause";
      copyright = "Copyright: (c) 2016-2021 Pasqualino `Titto` Assini";
      maintainer = "tittoassini@gmail.com";
      author = "Pasqualino `Titto` Assini";
      homepage = "http://quid2.org";
      url = "";
      synopsis = "Principled and efficient bit-oriented binary serialization.";
      description = "Reference implementation of `flat`, a principled and efficient binary serialization format.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = if compiler.isEta && true
          then [
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."dlist" or (errorHandler.buildDepError "dlist"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
            (hsPkgs."mono-traversable" or (errorHandler.buildDepError "mono-traversable"))
            (hsPkgs."pretty" or (errorHandler.buildDepError "pretty"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ]
          else [
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."dlist" or (errorHandler.buildDepError "dlist"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."mono-traversable" or (errorHandler.buildDepError "mono-traversable"))
            (hsPkgs."pretty" or (errorHandler.buildDepError "pretty"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."list-t" or (errorHandler.buildDepError "list-t"))
            ];
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = ([
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."quickcheck-text" or (errorHandler.buildDepError "quickcheck-text"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8") (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))) ++ (if compiler.isEta && true
            then [
              (hsPkgs."array" or (errorHandler.buildDepError "array"))
              (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
              (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
              (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
              (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
              (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
              (hsPkgs."mono-traversable" or (errorHandler.buildDepError "mono-traversable"))
              (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
              (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
              (hsPkgs."text" or (errorHandler.buildDepError "text"))
              ]
            else [
              (hsPkgs."array" or (errorHandler.buildDepError "array"))
              (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
              (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
              (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
              (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
              (hsPkgs."mono-traversable" or (errorHandler.buildDepError "mono-traversable"))
              (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
              (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
              (hsPkgs."text" or (errorHandler.buildDepError "text"))
              (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
              ]);
          buildable = true;
          };
        "doc-static" = {
          depends = [
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."dlist" or (errorHandler.buildDepError "dlist"))
            (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
            (hsPkgs."pretty" or (errorHandler.buildDepError "pretty"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8") (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
          buildable = true;
          };
        "Repr" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
            (hsPkgs."list-t" or (errorHandler.buildDepError "list-t"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/flat-0.4.4.0.0.0.0.2.tar.gz";
      sha256 = "e0f558a3832f094b3471ee1c9a6311128eaaa3f2a23b5b614d86f5496cea002f";
      });
    }) // {
    package-description-override = "cabal-version:      >=1.10\nname:               flat\nversion:            0.4.4.0.0.0.0.2\nlicense:            BSD3\nlicense-file:       LICENSE\ncopyright:          Copyright: (c) 2016-2021 Pasqualino `Titto` Assini\nmaintainer:         tittoassini@gmail.com\nauthor:             Pasqualino `Titto` Assini\ntested-with:\n    ghc ==7.10.3 || ==8.0.2 || ==8.2.2 || ==8.4.4 || ==8.6.5 || ==8.8.4 || ==8.10.3\n\nhomepage:           http://quid2.org\nsynopsis:           Principled and efficient bit-oriented binary serialization.\ndescription:\n    Reference implementation of `flat`, a principled and efficient binary serialization format.\n\ncategory:           Data,Parsing,Serialization\nbuild-type:         Simple\nextra-source-files:\n    stack.yaml\n    stack-6.35.yaml\n    stack-9.21.yaml\n    README.md\n    CHANGELOG\n\nsource-repository head\n    type:     git\n    location: https://github.com/Quid2/flat\n\nlibrary\n    exposed-modules:\n        Data.ByteString.Convert\n        Data.FloatCast\n        Data.ZigZag\n        Flat\n        Flat.Bits\n        Flat.Class\n        Flat.Decoder\n        Flat.Decoder.Prim\n        Flat.Decoder.Run\n        Flat.Decoder.Strict\n        Flat.Decoder.Types\n        Flat.Encoder\n        Flat.Encoder.Prim\n        Flat.Encoder.Size\n        Flat.Encoder.Strict\n        Flat.Encoder.Types\n        Flat.Endian\n        Flat.Filler\n        Flat.Instances\n        Flat.Instances.Array\n        Flat.Instances.Base\n        Flat.Instances.ByteString\n        Flat.Instances.Containers\n        Flat.Instances.DList\n        Flat.Instances.Mono\n        Flat.Instances.Test\n        Flat.Instances.Text\n        Flat.Instances.Unordered\n        Flat.Instances.Util\n        Flat.Instances.Vector\n        Flat.Memory\n        Flat.Run\n        Flat.Repr\n        Flat.Tutorial\n        Flat.Types\n\n    hs-source-dirs:   src\n    default-language: Haskell2010\n    other-extensions:\n        NoMonomorphismRestriction DataKinds DefaultSignatures\n        DeriveAnyClass DeriveFoldable DeriveFunctor DeriveGeneric\n        DeriveTraversable FlexibleContexts FlexibleInstances\n        OverloadedStrings PolyKinds ScopedTypeVariables TupleSections\n        TypeFamilies TypeOperators UndecidableInstances\n\n    ghc-options:\n        -Wall -O2 -funbox-strict-fields -fno-warn-orphans\n        -fno-warn-name-shadowing\n\n    if impl(eta >=0)\n        build-depends:\n            array ==0.5.2.0,\n            base,\n            bytestring ==0.10.8.2,\n            containers ==0.5.9.1,\n            deepseq ==1.4.3.0,\n            dlist,\n            filepath ==1.4.1.1,\n            ghc-prim ==0.4.0.0,\n            hashable >=1.2.4.0 && <=1.2.7.0,\n            HUnit ==1.6.0.0,\n            memory >=0.14.10 && <=0.14.14,\n            mono-traversable ==1.0.1,\n            pretty >=1.1.3.4 && <=1.1.3.6,\n            primitive >=0.6.1.0 && <=0.6.4.0,\n            QuickCheck ==2.10,\n            tasty ==1.1.0.3,\n            text ==1.2.3.0,\n            unordered-containers >=0.2.7.1 && <=0.2.9.0,\n            vector >=0.11.0.0 && <=0.12.0.1\n\n    else\n        build-depends:\n            array >=0.5.1.0,\n            base >=4.8 && <5,\n            bytestring >=0.10.6,\n            containers,\n            deepseq >=1.4,\n            dlist >=0.6,\n            ghc-prim,\n            hashable,\n            mono-traversable,\n            pretty >=1.1.2,\n            primitive,\n            semigroups,\n            text,\n            unordered-containers,\n            vector,\n            QuickCheck,\n            quickcheck-instances,\n            list-t\n\ntest-suite spec\n    type:             exitcode-stdio-1.0\n    main-is:          Spec.hs\n    cpp-options:      -DLIST_BIT -DTEST_DECBITS\n    hs-source-dirs:   test\n    other-modules:\n        Test.Data\n        Test.Data.Arbitrary\n        Test.Data.Flat\n        Test.Data.Values\n        Test.Data2\n        Test.Data2.Flat\n        Test.E\n        Test.E.Arbitrary\n        Test.E.Flat\n\n    default-language: Haskell2010\n    build-depends:\n        base,\n        flat,\n        ghc-prim,\n        quickcheck-text,\n        tasty-hunit,\n        tasty-quickcheck,\n        text\n\n    if impl(ghc <8.6)\n        cpp-options: -DENUM_LARGE\n\n    if impl(ghc >8)\n        ghc-options:\n            -Wno-unused-top-binds -Wno-type-defaults -Wno-missing-signatures\n\n    if impl(ghc <8)\n        build-depends: semigroups\n\n    if impl(eta >=0)\n        build-depends:\n            array ==0.5.2.0,\n            bytestring ==0.10.8.2,\n            containers ==0.5.9.1,\n            deepseq ==1.4.3.0,\n            filepath ==1.4.1.1,\n            HUnit ==1.6.0.0,\n            mono-traversable ==1.0.1,\n            QuickCheck ==2.10,\n            tasty ==1.1.0.3,\n            text ==1.2.3.0\n\n    else\n        build-depends:\n            array,\n            bytestring,\n            containers,\n            deepseq,\n            filepath,\n            mono-traversable,\n            QuickCheck,\n            tasty,\n            text,\n            quickcheck-instances\n\ntest-suite doc-static\n    type:             exitcode-stdio-1.0\n    main-is:          DocTests.hs\n    hs-source-dirs:   test\n    other-modules:\n        DocTest\n        DocTest.Data.FloatCast\n        DocTest.Data.ZigZag\n        DocTest.Flat.Bits\n        DocTest.Flat.Decoder.Prim\n        DocTest.Flat.Endian\n        DocTest.Flat.Instances.Array\n        DocTest.Flat.Instances.Base\n        DocTest.Flat.Instances.ByteString\n        DocTest.Flat.Instances.Containers\n        DocTest.Flat.Instances.DList\n        DocTest.Flat.Instances.Mono\n        DocTest.Flat.Instances.Text\n        DocTest.Flat.Instances.Unordered\n        DocTest.Flat.Instances.Vector\n        DocTest.Flat.Tutorial\n\n    default-language: Haskell2010\n    build-depends:\n        array,\n        base,\n        bytestring,\n        containers,\n        dlist,\n        flat,\n        pretty,\n        quickcheck-instances,\n        tasty,\n        tasty-hunit,\n        tasty-quickcheck,\n        text,\n        unordered-containers,\n        vector\n\n    if impl(ghc <8)\n        build-depends: semigroups\n\ntest-suite Repr\n    type:             exitcode-stdio-1.0\n    main-is:          FlatRepr.hs\n    hs-source-dirs:   test\n    default-language: Haskell2010\n    ghc-options:      -rtsopts\n    build-depends:\n        base,\n        bytestring,\n        flat,\n        list-t\n";
    }