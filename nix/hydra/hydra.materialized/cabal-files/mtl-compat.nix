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
    flags = { two-point-one = false; two-point-two = false; };
    package = {
      specVersion = "1.8";
      identifier = { name = "mtl-compat"; version = "0.2.2"; };
      license = "BSD-3-Clause";
      copyright = "(C) 2015-2017 Ryan Scott";
      maintainer = "Ryan Scott <ryan.gl.scott@gmail.com>";
      author = "Ryan Scott";
      homepage = "https://github.com/haskell-compat/mtl-compat";
      url = "";
      synopsis = "Backported Control.Monad.Except module from mtl";
      description = "This package backports the \"Control.Monad.Except\" module from\n@mtl@ (if using @mtl-2.2.0.1@ or earlier), which reexports the\n@ExceptT@ monad transformer and the @MonadError@ class.\n\nThis package should only be used if there is a need to use the\n@Control.Monad.Except@ module specifically. If you just want\nthe @mtl@ class instances for @ExceptT@, use\n@transformers-compat@ instead, since @mtl-compat@ does nothing\nbut reexport the instances from that package.\n\nNote that unlike how @mtl-2.2@ or later works, the\n\"Control.Monad.Except\" module defined in this package exports\nall of @ExceptT@'s monad class instances. Therefore, you may\nhave to declare @import Control.Monad.Except ()@ at the top of\nyour file to get all of the @ExceptT@ instances in scope.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ] ++ (if flags.two-point-two
          then [
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ]
          else [
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            ])) ++ (if flags.two-point-one
          then [
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
            ]
          else [ (hsPkgs."mtl" or (errorHandler.buildDepError "mtl")) ]);
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/mtl-compat-0.2.2.tar.gz";
      sha256 = "1955398fe2115674f47f553b2caaf928c6aa3424271a5cd13bc191e54bfe3a9e";
      });
    }) // {
    package-description-override = "name:                mtl-compat\nversion:             0.2.2\nsynopsis:            Backported Control.Monad.Except module from mtl\ndescription:         This package backports the \"Control.Monad.Except\" module from\n                     @mtl@ (if using @mtl-2.2.0.1@ or earlier), which reexports the\n                     @ExceptT@ monad transformer and the @MonadError@ class.\n                     .\n                     This package should only be used if there is a need to use the\n                     @Control.Monad.Except@ module specifically. If you just want\n                     the @mtl@ class instances for @ExceptT@, use\n                     @transformers-compat@ instead, since @mtl-compat@ does nothing\n                     but reexport the instances from that package.\n                     .\n                     Note that unlike how @mtl-2.2@ or later works, the\n                     \"Control.Monad.Except\" module defined in this package exports\n                     all of @ExceptT@'s monad class instances. Therefore, you may\n                     have to declare @import Control.Monad.Except ()@ at the top of\n                     your file to get all of the @ExceptT@ instances in scope.\nhomepage:            https://github.com/haskell-compat/mtl-compat\nbug-reports:         https://github.com/haskell-compat/mtl-compat/issues\nstability:           Provisional\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              Ryan Scott\nmaintainer:          Ryan Scott <ryan.gl.scott@gmail.com>\ncopyright:           (C) 2015-2017 Ryan Scott\ncategory:            Compatibility\nbuild-type:          Simple\nextra-source-files:  CHANGELOG.md, README.md\ntested-with:         GHC == 7.0.4\n                   , GHC == 7.2.2\n                   , GHC == 7.4.2\n                   , GHC == 7.6.3\n                   , GHC == 7.8.4\n                   , GHC == 7.10.3\n                   , GHC == 8.0.2\n                   , GHC == 8.2.2\n                   , GHC == 8.4.4\n                   , GHC == 8.6.5\n                   , GHC == 8.8.1\ncabal-version:       >=1.8\n\nsource-repository head\n  type:                git\n  location:            https://github.com/haskell-compat/mtl-compat\n\nflag two-point-one\n  default:             False\n  manual:              False\n  description:         Use mtl-2.1.3.1 or earlier with transformers-compat. This\n                       will cause this package to export the Control.Monad.Except module.\n\nflag two-point-two\n  default:             False\n  manual:              False\n  description:         Use mtl-2.2.0.1 with transformers. This will cause this\n                       package to export the Control.Monad.Except module.\n\nlibrary\n  build-depends:       base                >= 4.3     && < 5\n\n  if flag(two-point-one) || flag(two-point-two)\n    exposed-modules:   Control.Monad.Except\n    hs-source-dirs:    src\n\n  if flag(two-point-two)\n    build-depends:   mtl                   >= 2.2.0.1 &&  < 2.2.1\n                   , transformers          >= 0.4.1   &&  < 0.6\n  else\n    build-depends:   mtl                    < 2.2.0.1 || >= 2.2.1\n\n  if flag(two-point-one)\n    build-depends:     mtl                 >= 2.0.1   &&  < 2.2\n                     , transformers-compat >= 0.4     &&  < 0.7\n  else\n    build-depends:     mtl                 >= 2.2\n\n  ghc-options:         -Wall\n";
    }