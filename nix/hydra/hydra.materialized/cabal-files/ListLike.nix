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
      identifier = { name = "ListLike"; version = "4.7.7"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (c) 2007-2008 John Goerzen";
      maintainer = "David Fox <dsf@seereason.com>, Andreas Abel";
      author = "John Goerzen";
      homepage = "http://github.com/ddssff/listlike";
      url = "";
      synopsis = "Generalized support for list-like structures";
      description = "Generalized support for list-like structures in Haskell.\n\nThe ListLike module provides a common interface to the various Haskell\ntypes that are list-like.  Predefined interfaces include standard\nHaskell lists, Arrays, ByteStrings, and lazy ByteStrings.  Custom\ntypes can easily be made ListLike instances as well.\n\nListLike also provides for String-like types, such as String and\nByteString, for types that support input and output, and for types that can handle\ninfinite lists.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."dlist" or (errorHandler.buildDepError "dlist"))
          (hsPkgs."fmlist" or (errorHandler.buildDepError "fmlist"))
          (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.4")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
        buildable = true;
        };
      tests = {
        "listlike-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."ListLike" or (errorHandler.buildDepError "ListLike"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."dlist" or (errorHandler.buildDepError "dlist"))
            (hsPkgs."fmlist" or (errorHandler.buildDepError "fmlist"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
            ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.4")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/ListLike-4.7.7.tar.gz";
      sha256 = "0a01cd234aa4a1e97c07ce09e24e6c95f6bb423add96d387fb5bd2e1f2779e40";
      });
    }) // {
    package-description-override = "Name: ListLike\r\nVersion: 4.7.7\r\nx-revision: 1\r\nLicense: BSD3\r\nMaintainer: David Fox <dsf@seereason.com>, Andreas Abel\r\nAuthor: John Goerzen\r\nCopyright: Copyright (c) 2007-2008 John Goerzen\r\nlicense-file: COPYRIGHT\r\nCategory: list, string, text, bytestring, vector\r\nCabal-Version: >= 1.10\r\nBuild-Type: Simple\r\nhomepage: http://github.com/ddssff/listlike\r\nsynopsis: Generalized support for list-like structures\r\nDescription: Generalized support for list-like structures in Haskell.\r\n .\r\n The ListLike module provides a common interface to the various Haskell\r\n types that are list-like.  Predefined interfaces include standard\r\n Haskell lists, Arrays, ByteStrings, and lazy ByteStrings.  Custom\r\n types can easily be made ListLike instances as well.\r\n .\r\n ListLike also provides for String-like types, such as String and\r\n ByteString, for types that support input and output, and for types that can handle\r\n infinite lists.\r\nStability: Stable\r\n\r\nTested-With:\r\n  GHC == 9.4.1\r\n  GHC == 9.2.2\r\n  GHC == 9.0.2\r\n  GHC == 8.10.7\r\n  GHC == 8.8.4\r\n  GHC == 8.6.5\r\n  GHC == 8.4.4\r\n  GHC == 8.2.2\r\n  GHC == 8.0.2\r\n  GHC == 7.10.3\r\n\r\nextra-source-files:\r\n  README.md\r\n  CHANGELOG.md\r\n\r\nLibrary\r\n  default-language: Haskell2010\r\n  GHC-Options: -Wall\r\n  Hs-Source-Dirs: src\r\n  Exposed-Modules: Data.ListLike\r\n          Data.ListLike.Base\r\n          Data.ListLike.Chars\r\n          Data.ListLike.CharString\r\n          Data.ListLike.FoldableLL\r\n          Data.ListLike.IO\r\n          Data.ListLike.Instances\r\n          Data.ListLike.String\r\n          Data.ListLike.Text\r\n          Data.ListLike.Text.Builder\r\n          Data.ListLike.Text.Text\r\n          Data.ListLike.Text.TextLazy\r\n          Data.ListLike.UTF8\r\n          Data.ListLike.Utils\r\n          Data.ListLike.Vector\r\n          Data.ListLike.Vector.Generic\r\n          Data.ListLike.Vector.Storable\r\n          Data.ListLike.Vector.Unboxed\r\n          Data.ListLike.Vector.Vector\r\n          Data.ListLike.DList\r\n          Data.ListLike.FMList\r\n  -- Other-Modules: Data.ConfigFile.Lexer\r\n  Build-Depends: base       >= 4.8   && < 5\r\n                ,containers >= 0.3   && < 0.7\r\n                ,bytestring >= 0.9.1 && < 0.12\r\n                ,array      >= 0.3   && < 0.6\r\n                ,text       >= 0.11  && < 1.3  || == 2.0.*\r\n                ,vector     >= 0.5   && < 0.14\r\n                ,dlist      >= 0.7   && < 1.1\r\n                ,fmlist     >= 0.8   && < 0.10\r\n                ,utf8-string >= 0.3.1 && < 1.1\r\n                ,deepseq\r\n\r\n  -- Remark: Atm, we don't comply with the Haskell Package Versioning Policy\r\n  --   https://pvp.haskell.org/\r\n  -- > §6. Client defines orphan instance.\r\n  -- > If a package defines an orphan instance, it MUST depend on the\r\n  -- > minor version of the packages that define the data type and the\r\n  -- > type class to be backwards compatible. For example,\r\n  -- > build-depends: mypkg >= 2.1.1 && < 2.1.2.\r\n  --\r\n  -- Since ListLike defines orphan instances, we would need to include\r\n  -- the minor version number in the upper bounds.\r\n  -- (See issues #7 and #10.)\r\n  -- However, this could involve a maintenance marathon to relax upper bounds.\r\n\r\n  If !impl(ghc >= 8.4)\r\n    Build-Depends: semigroups >= 0.16 && < 0.20\r\n\r\n  if impl(ghc >= 8.0)\r\n    ghc-options:  -Wcompat\r\n\r\nTest-suite listlike-tests\r\n  default-language: Haskell2010\r\n  Hs-source-dirs: testsrc\r\n  Main-is:        runtests.hs\r\n  Type:           exitcode-stdio-1.0\r\n\r\n  Other-modules:  TestInfrastructure\r\n  Build-depends:   base\r\n                  ,ListLike\r\n                  ,HUnit      >= 1.2 && < 2\r\n                  ,QuickCheck >= 2.4 && < 3\r\n                  ,random     >= 1   && < 2\r\n                  ,array\r\n                  ,bytestring\r\n                  ,containers\r\n                  ,dlist\r\n                  ,fmlist\r\n                  ,text\r\n                  ,vector\r\n                  ,utf8-string\r\n  If !impl(ghc >= 8.4)\r\n    Build-Depends: semigroups >= 0.16 && < 0.20\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: git://github.com/ddssff/listlike.git\r\n";
    }