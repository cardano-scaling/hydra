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
      identifier = { name = "reducers"; version = "3.12.4"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (C) 2008-2016 Edward A. Kmett";
      maintainer = "Edward A. Kmett <ekmett@gmail.com>";
      author = "Edward A. Kmett";
      homepage = "http://github.com/ekmett/reducers/";
      url = "";
      synopsis = "Semigroups, specialized containers and a general map/reduce framework";
      description = "Semigroups, specialized containers and a general map/reduce framework.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."fingertree" or (errorHandler.buildDepError "fingertree"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."semigroupoids" or (errorHandler.buildDepError "semigroupoids"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.0") (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/reducers-3.12.4.tar.gz";
      sha256 = "c066f545df7947613217256d210ecb59ba709294e7b06dad8c4d8a9263635e43";
      });
    }) // {
    package-description-override = "name:          reducers\r\ncategory:      Data, Math, Numerical, Semigroups\r\nversion:       3.12.4\r\nx-revision: 2\r\nlicense:       BSD3\r\ncabal-version: >= 1.10\r\nlicense-file:  LICENSE\r\nauthor:        Edward A. Kmett\r\nmaintainer:    Edward A. Kmett <ekmett@gmail.com>\r\nstability:     provisional\r\nhomepage:      http://github.com/ekmett/reducers/\r\nbug-reports:   http://github.com/ekmett/reducers/issues\r\ncopyright:     Copyright (C) 2008-2016 Edward A. Kmett\r\nsynopsis:      Semigroups, specialized containers and a general map/reduce framework\r\ndescription:   Semigroups, specialized containers and a general map/reduce framework.\r\nbuild-type:    Simple\r\nextra-source-files: CHANGELOG.markdown README.markdown\r\ntested-with:   GHC == 7.0.4\r\n             , GHC == 7.2.2\r\n             , GHC == 7.4.2\r\n             , GHC == 7.6.3\r\n             , GHC == 7.8.4\r\n             , GHC == 7.10.3\r\n             , GHC == 8.0.2\r\n             , GHC == 8.2.2\r\n             , GHC == 8.4.4\r\n             , GHC == 8.6.5\r\n             , GHC == 8.8.4\r\n             , GHC == 8.10.7\r\n             , GHC == 9.0.1\r\n             , GHC == 9.2.1\r\n\r\nsource-repository head\r\n  type: git\r\n  location: git://github.com/ekmett/reducers.git\r\n\r\nlibrary\r\n  hs-source-dirs: src\r\n\r\n  build-depends:\r\n    base                   >= 4        && < 5,\r\n    array                  >= 0.3      && < 0.6,\r\n    transformers           >= 0.2      && < 0.7,\r\n    bytestring             >= 0.9.1    && < 0.12,\r\n    containers             >= 0.3      && < 0.7,\r\n    fingertree             >= 0.1      && < 0.2,\r\n    hashable               >= 1.1.2.1  && < 1.5,\r\n    text                   >= 0.11.1.5 && < 2.1,\r\n    unordered-containers   >= 0.2      && < 0.3,\r\n    semigroupoids          >= 4        && < 6\r\n\r\n  if impl(ghc < 8.0)\r\n    build-depends:\r\n      semigroups           >= 0.9      && < 1\r\n\r\n  exposed-modules:\r\n    Data.Generator\r\n    Data.Generator.Combinators\r\n    Data.Semigroup.Generator\r\n    Data.Semigroup.Reducer\r\n    Data.Semigroup.Reducer.With\r\n    Data.Semigroup.Instances\r\n    Data.Semigroup.Union\r\n    Data.Semigroup.Apply\r\n    Data.Semigroup.Applicative\r\n    Data.Semigroup.Alt\r\n    Data.Semigroup.Alternative\r\n    Data.Semigroup.Monad\r\n    Data.Semigroup.MonadPlus\r\n    Data.Semigroup.Self\r\n\r\n  if impl(ghc)\r\n    default-extensions: DeriveDataTypeable\r\n    cpp-options: -DLANGUAGE_DeriveDataTypeable\r\n\r\n  ghc-options: -Wall\r\n  default-language: Haskell2010\r\n\r\n  -- hack around the buggy unused matches check for class associated types in ghc 8 rc1\r\n  if impl(ghc >= 8)\r\n    ghc-options: -fno-warn-unused-matches\r\n\r\n";
    }