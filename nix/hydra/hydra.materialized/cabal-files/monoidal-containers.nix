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
    flags = { split-these = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "monoidal-containers"; version = "0.6.3.0"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2014 Ben Gamari";
      maintainer = "ben@smart-cactus.org";
      author = "Ben Gamari";
      homepage = "http://github.com/bgamari/monoidal-containers";
      url = "";
      synopsis = "Containers with monoidal accumulation";
      description = "Containers with merging via monoidal accumulation. The 'Monoid' instances\nprovided by the @containers@ and @unordered-containers@ packages merge\nstructures in a left-biased manner instead of using the underlying monoidal\nstructure of the value.\n\nThis package wraps the types provided by these packages, but provides @Monoid@\ninstances implemented in terms of the value type's 'mappend'. For instance,\nthe @Monoid@ @Map@ instance looks like,\n\n@\ninstance (Ord k, Semigroup a) => Monoid (MonoidalMap k a)\n@";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."newtype" or (errorHandler.buildDepError "newtype"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."witherable" or (errorHandler.buildDepError "witherable"))
          ] ++ (if flags.split-these
          then [
            (hsPkgs."semialign" or (errorHandler.buildDepError "semialign"))
            (hsPkgs."these" or (errorHandler.buildDepError "these"))
            ]
          else [ (hsPkgs."these" or (errorHandler.buildDepError "these")) ]);
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/monoidal-containers-0.6.3.0.tar.gz";
      sha256 = "047e86f1d31c56c8aee60eaff340b57340b1aa9a29f9ecf64679cb9141f98154";
      });
    }) // {
    package-description-override = "name:               monoidal-containers\nversion:            0.6.3.0\nsynopsis:           Containers with monoidal accumulation\ndescription:\n  Containers with merging via monoidal accumulation. The 'Monoid' instances\n  provided by the @containers@ and @unordered-containers@ packages merge\n  structures in a left-biased manner instead of using the underlying monoidal\n  structure of the value.\n  .\n  This package wraps the types provided by these packages, but provides @Monoid@\n  instances implemented in terms of the value type's 'mappend'. For instance,\n  the @Monoid@ @Map@ instance looks like,\n  .\n  @\n  instance (Ord k, Semigroup a) => Monoid (MonoidalMap k a)\n  @\n\nhomepage:           http://github.com/bgamari/monoidal-containers\nlicense:            BSD3\nlicense-file:       LICENSE\nauthor:             Ben Gamari\nmaintainer:         ben@smart-cactus.org\ncopyright:          (c) 2014 Ben Gamari\ncategory:           Data\nbuild-type:         Simple\ncabal-version:      >=1.10\nextra-source-files: Changelog.md\ntested-with:\n  GHC ==8.2.2 || ==8.4.4 || ==8.6.5 || ==8.8.4 || ==8.10.2 || ==9.0.2 || ==9.2.1 || ==9.4.1\n\nsource-repository head\n  type:     git\n  location: git://github.com/bgamari/monoidal-containers\n\nflag split-these\n  description: Use split these/semialign packages\n  manual:      False\n  default:     True\n\nlibrary\n  exposed-modules:\n    Data.HashMap.Monoidal\n    Data.IntMap.Monoidal\n    Data.IntMap.Monoidal.Strict\n    Data.Map.Monoidal\n    Data.Map.Monoidal.Strict\n\n  other-extensions:\n    CPP\n    DeriveDataTypeable\n    DeriveTraversable\n    GeneralizedNewtypeDeriving\n    MultiParamTypeClasses\n\n  build-depends:\n      aeson                 >=1.0   && <2.2\n    , base                  >=4.7   && <4.18\n    , containers            >=0.5.9 && <0.7\n    , deepseq               >=1.3   && <1.5\n    , hashable              >=1.2   && <1.5\n    , lens                  >=4.4   && <5.3\n    , newtype               >=0.2   && <0.3\n    , unordered-containers  >=0.2   && <0.3\n    , witherable            >=0.4   && <0.5\n\n  if flag(split-these)\n    build-depends:\n        semialign  >=1 && <1.3\n      , these      >=1 && <1.2\n\n  else\n    build-depends: these >=0.7 && <0.9\n\n  hs-source-dirs:   src\n  default-language: Haskell2010\n";
    }