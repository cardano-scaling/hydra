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
      identifier = { name = "mmorph"; version = "1.2.0"; };
      license = "BSD-3-Clause";
      copyright = "2013 Gabriella Gonzalez";
      maintainer = "GenuineGabriella@gmail.com";
      author = "Gabriella Gonzalez";
      homepage = "";
      url = "";
      synopsis = "Monad morphisms";
      description = "This library provides monad morphism utilities, most commonly used\nfor manipulating monad transformer stacks.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.0") (hsPkgs."fail" or (errorHandler.buildDepError "fail"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/mmorph-1.2.0.tar.gz";
      sha256 = "61338058eb676b466a462ca45d59f436a77a3bd6b816e4268c6d88522b6a4280";
      });
    }) // {
    package-description-override = "Name: mmorph\r\nVersion: 1.2.0\r\nx-revision: 3\r\nCabal-Version: >= 1.10\r\nBuild-Type: Simple\r\nLicense: BSD3\r\nLicense-File: LICENSE\r\nCopyright: 2013 Gabriella Gonzalez\r\nAuthor: Gabriella Gonzalez\r\nMaintainer: GenuineGabriella@gmail.com\r\nBug-Reports: https://github.com/Gabriella439/Haskell-MMorph-Library/issues\r\nSynopsis: Monad morphisms\r\nDescription: This library provides monad morphism utilities, most commonly used\r\n    for manipulating monad transformer stacks.\r\nCategory: Control\r\nExtra-Source-Files: CHANGELOG.md\r\nSource-Repository head\r\n    Type: git\r\n    Location: https://github.com/Gabriella439/Haskell-MMorph-Library\r\n\r\nLibrary\r\n    Hs-Source-Dirs: src\r\n    Build-Depends:\r\n        base                >= 4.5     && < 5  ,\r\n        mtl                 >= 2.1     && < 2.4,\r\n        transformers        >= 0.2.0.0 && < 0.7,\r\n        transformers-compat >= 0.3     && < 0.8\r\n    if impl(ghc < 8.0)\r\n        Build-Depends:\r\n            fail == 4.9.*\r\n    Exposed-Modules: Control.Monad.Morph, Control.Monad.Trans.Compose\r\n    GHC-Options: -O2\r\n    Default-Language: Haskell2010\r\n";
    }