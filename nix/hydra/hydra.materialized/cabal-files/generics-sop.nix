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
      identifier = { name = "generics-sop"; version = "0.5.1.2"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "andres@well-typed.com";
      author = "Edsko de Vries <edsko@well-typed.com>, Andres Löh <andres@well-typed.com>";
      homepage = "";
      url = "";
      synopsis = "Generic Programming using True Sums of Products";
      description = "A library to support the definition of generic functions.\nDatatypes are viewed in a uniform, structured way:\nthe choice between constructors is represented using an n-ary\nsum, and the arguments of each constructor are represented using\nan n-ary product.\n\nThe module \"Generics.SOP\" is the main module of this library and contains\nmore detailed documentation.\n\nSince version 0.4.0.0, this package is now based on\n@<https://hackage.haskell.org/package/sop-core sop-core>@. The core package\ncontains all the functionality of n-ary sums and products, whereas this\npackage provides the datatype-generic programming support on top.\n\nExamples of using this library are provided by the following\npackages:\n\n* @<https://hackage.haskell.org/package/basic-sop basic-sop>@ basic examples,\n\n* @<https://hackage.haskell.org/package/pretty-sop pretty-sop>@ generic pretty printing,\n\n* @<https://hackage.haskell.org/package/lens-sop lens-sop>@ generically computed lenses,\n\n* @<https://hackage.haskell.org/package/json-sop json-sop>@ generic JSON conversions.\n\nA detailed description of the ideas behind this library is provided by\nthe paper:\n\n* Edsko de Vries and Andres Löh.\n<http://www.andres-loeh.de/TrueSumsOfProducts True Sums of Products>.\nWorkshop on Generic Programming (WGP) 2014.\n";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."sop-core" or (errorHandler.buildDepError "sop-core"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."th-abstraction" or (errorHandler.buildDepError "th-abstraction"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          ];
        buildable = true;
        };
      tests = {
        "generics-sop-examples" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."generics-sop" or (errorHandler.buildDepError "generics-sop"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "generics-sop-bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."generics-sop" or (errorHandler.buildDepError "generics-sop"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/generics-sop-0.5.1.2.tar.gz";
      sha256 = "639d4140520d608a73a81186aca6db40fcb03ec8a818dbce79fcb0b39aa70b25";
      });
    }) // {
    package-description-override = "name:                generics-sop\nversion:             0.5.1.2\nx-revision: 1\nsynopsis:            Generic Programming using True Sums of Products\ndescription:\n  A library to support the definition of generic functions.\n  Datatypes are viewed in a uniform, structured way:\n  the choice between constructors is represented using an n-ary\n  sum, and the arguments of each constructor are represented using\n  an n-ary product.\n  .\n  The module \"Generics.SOP\" is the main module of this library and contains\n  more detailed documentation.\n  .\n  Since version 0.4.0.0, this package is now based on\n  @<https://hackage.haskell.org/package/sop-core sop-core>@. The core package\n  contains all the functionality of n-ary sums and products, whereas this\n  package provides the datatype-generic programming support on top.\n  .\n  Examples of using this library are provided by the following\n  packages:\n  .\n    * @<https://hackage.haskell.org/package/basic-sop basic-sop>@ basic examples,\n  .\n    * @<https://hackage.haskell.org/package/pretty-sop pretty-sop>@ generic pretty printing,\n  .\n    * @<https://hackage.haskell.org/package/lens-sop lens-sop>@ generically computed lenses,\n  .\n    * @<https://hackage.haskell.org/package/json-sop json-sop>@ generic JSON conversions.\n  .\n  A detailed description of the ideas behind this library is provided by\n  the paper:\n  .\n    * Edsko de Vries and Andres Löh.\n      <http://www.andres-loeh.de/TrueSumsOfProducts True Sums of Products>.\n      Workshop on Generic Programming (WGP) 2014.\n  .\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              Edsko de Vries <edsko@well-typed.com>, Andres Löh <andres@well-typed.com>\nmaintainer:          andres@well-typed.com\ncategory:            Generics\nbuild-type:          Simple\ncabal-version:       >=1.10\nextra-source-files:  CHANGELOG.md doctest.sh\ntested-with:         GHC == 8.0.2, GHC == 8.2.2, GHC == 8.4.4, GHC == 8.6.5, GHC == 8.8.2, GHC == 8.10.7, GHC == 9.0.2, GHC == 9.2.4, GHC == 9.4.1\n\nsource-repository head\n  type:                git\n  location:            https://github.com/well-typed/generics-sop\n\nlibrary\n  exposed-modules:     Generics.SOP\n                       Generics.SOP.GGP\n                       Generics.SOP.TH\n                       Generics.SOP.Type.Metadata\n                       -- exposed via Generics.SOP:\n                       Generics.SOP.Instances\n                       Generics.SOP.Metadata\n                       Generics.SOP.Universe\n                       -- re-exported from Data.SOP:\n                       Generics.SOP.Dict\n                       Generics.SOP.BasicFunctors\n                       Generics.SOP.Classes\n                       Generics.SOP.Constraint\n                       Generics.SOP.NP\n                       Generics.SOP.NS\n                       Generics.SOP.Sing\n  build-depends:       base                 >= 4.9  && < 4.18,\n                       sop-core             == 0.5.0.*,\n                       template-haskell     >= 2.8  && < 2.20,\n                       th-abstraction       >= 0.4  && < 0.5,\n                       ghc-prim             >= 0.3  && < 0.10\n  hs-source-dirs:      src\n  default-language:    Haskell2010\n  ghc-options:         -Wall\n  default-extensions:  CPP\n                       ScopedTypeVariables\n                       TypeFamilies\n                       RankNTypes\n                       TypeOperators\n                       GADTs\n                       ConstraintKinds\n                       MultiParamTypeClasses\n                       TypeSynonymInstances\n                       FlexibleInstances\n                       FlexibleContexts\n                       DeriveFunctor\n                       DeriveFoldable\n                       DeriveTraversable\n                       DefaultSignatures\n                       KindSignatures\n                       DataKinds\n                       FunctionalDependencies\n\n  if impl(ghc <8.2)\n    default-extensions: AutoDeriveTypeable\n\n  -- if impl(ghc >= 8.6)\n  --   default-extensions: NoStarIsType\n  other-extensions:    PolyKinds\n                       UndecidableInstances\n                       TemplateHaskell\n                       StandaloneDeriving\n                       EmptyCase\n                       UndecidableSuperClasses\n\ntest-suite generics-sop-examples\n  type:                exitcode-stdio-1.0\n  main-is:             Example.hs\n  other-modules:       HTransExample\n  hs-source-dirs:      test\n  default-language:    Haskell2010\n  ghc-options:         -Wall\n  build-depends:       base                 >= 4.9  && < 5,\n                       generics-sop\n  other-extensions:    DeriveGeneric\n                       EmptyCase\n                       TemplateHaskell\n                       ConstraintKinds\n                       GADTs\n                       DataKinds\n                       TypeFamilies\n                       FlexibleContexts\n                       FlexibleInstances\n                       PolyKinds\n                       DefaultSignatures\n                       FunctionalDependencies\n                       MultiParamTypeClasses\n                       TypeFamilies\n\nbenchmark generics-sop-bench\n  type:                exitcode-stdio-1.0\n  main-is:             SOPBench.hs\n  other-modules:       SOPBench.Type\n                       SOPBench.Roundtrip\n                       SOPBench.Eq\n                       SOPBench.Show\n  hs-source-dirs:      bench\n  default-language:    Haskell2010\n  ghc-options:         -Wall\n  build-depends:       base                 >= 4.6  && < 5,\n                       criterion,\n                       deepseq,\n                       generics-sop,\n                       template-haskell\n";
    }