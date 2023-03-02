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
      specVersion = "1.12";
      identifier = { name = "hspec-junit-formatter"; version = "1.1.0.2"; };
      license = "MIT";
      copyright = "2021 Renaissance Learning Inc";
      maintainer = "engineering@freckle.com";
      author = "Freckle R&D";
      homepage = "https://github.com/freckle/hspec-junit-formatter#readme";
      url = "";
      synopsis = "A JUnit XML runner/formatter for hspec";
      description = "Allows hspec tests to write JUnit XML output for parsing in various tools.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."hspec-core" or (errorHandler.buildDepError "hspec-core"))
          (hsPkgs."iso8601-time" or (errorHandler.buildDepError "iso8601-time"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."xml-conduit" or (errorHandler.buildDepError "xml-conduit"))
          (hsPkgs."xml-types" or (errorHandler.buildDepError "xml-types"))
          ];
        buildable = true;
        };
      tests = {
        "readme" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."hspec-junit-formatter" or (errorHandler.buildDepError "hspec-junit-formatter"))
            (hsPkgs."markdown-unlit" or (errorHandler.buildDepError "markdown-unlit"))
            ];
          buildable = true;
          };
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."hspec-junit-formatter" or (errorHandler.buildDepError "hspec-junit-formatter"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."xml-conduit" or (errorHandler.buildDepError "xml-conduit"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/hspec-junit-formatter-1.1.0.2.tar.gz";
      sha256 = "2f002c6731e877cc9b044c975905c6ad4eeacbcb1432daa37e849f287574efbf";
      });
    }) // {
    package-description-override = "cabal-version:      1.12\nname:               hspec-junit-formatter\nversion:            1.1.0.2\nlicense:            MIT\nlicense-file:       LICENSE\ncopyright:          2021 Renaissance Learning Inc\nmaintainer:         engineering@freckle.com\nauthor:             Freckle R&D\nhomepage:           https://github.com/freckle/hspec-junit-formatter#readme\nbug-reports:        https://github.com/freckle/hspec-junit-formatter/issues\nsynopsis:           A JUnit XML runner/formatter for hspec\ndescription:\n    Allows hspec tests to write JUnit XML output for parsing in various tools.\n\ncategory:           Testing\nbuild-type:         Simple\nextra-source-files:\n    README.md\n    CHANGELOG.md\n    tests/golden.xml\n    tests/golden-prefixed.xml\n\nsource-repository head\n    type:     git\n    location: https://github.com/freckle/hspec-junit-formatter\n\nlibrary\n    exposed-modules:\n        Test.Hspec.Core.Runner.Ext\n        Test.Hspec.JUnit\n        Test.Hspec.JUnit.Config\n        Test.Hspec.JUnit.Config.Env\n        Test.Hspec.JUnit.Render\n        Test.Hspec.JUnit.Schema\n\n    hs-source-dirs:     library\n    other-modules:      Paths_hspec_junit_formatter\n    default-language:   Haskell2010\n    default-extensions:\n        BangPatterns DeriveAnyClass DeriveFoldable DeriveFunctor\n        DeriveGeneric DeriveLift DeriveTraversable DerivingStrategies\n        FlexibleContexts FlexibleInstances GADTs GeneralizedNewtypeDeriving\n        LambdaCase MultiParamTypeClasses NoImplicitPrelude\n        NoMonomorphismRestriction OverloadedStrings RankNTypes\n        RecordWildCards ScopedTypeVariables StandaloneDeriving\n        TypeApplications TypeFamilies\n\n    build-depends:\n        base >=4.11.1.0 && <5,\n        conduit >=1.3.1,\n        containers >=0.5.11.0,\n        directory >=1.3.1.5,\n        exceptions >=0.10.0,\n        filepath >=1.4.2,\n        hspec-core >=2.8.1,\n        iso8601-time >=0.1.5,\n        text >=1.2.3.1,\n        time >=1.8.0.2,\n        xml-conduit >=1.8.0.1,\n        xml-types >=0.3.6\n\ntest-suite readme\n    type:               exitcode-stdio-1.0\n    main-is:            README.lhs\n    other-modules:      Paths_hspec_junit_formatter\n    default-language:   Haskell2010\n    default-extensions:\n        BangPatterns DeriveAnyClass DeriveFoldable DeriveFunctor\n        DeriveGeneric DeriveLift DeriveTraversable DerivingStrategies\n        FlexibleContexts FlexibleInstances GADTs GeneralizedNewtypeDeriving\n        LambdaCase MultiParamTypeClasses NoImplicitPrelude\n        NoMonomorphismRestriction OverloadedStrings RankNTypes\n        RecordWildCards ScopedTypeVariables StandaloneDeriving\n        TypeApplications TypeFamilies\n\n    ghc-options:        -pgmL markdown-unlit\n    build-depends:\n        base >=4.11.1.0 && <5,\n        hspec >=2.8.1,\n        hspec-junit-formatter -any,\n        markdown-unlit >=0.5.0\n\ntest-suite spec\n    type:               exitcode-stdio-1.0\n    main-is:            Main.hs\n    hs-source-dirs:     tests\n    other-modules:\n        ExampleSpec\n        Paths_hspec_junit_formatter\n\n    default-language:   Haskell2010\n    default-extensions:\n        BangPatterns DeriveAnyClass DeriveFoldable DeriveFunctor\n        DeriveGeneric DeriveLift DeriveTraversable DerivingStrategies\n        FlexibleContexts FlexibleInstances GADTs GeneralizedNewtypeDeriving\n        LambdaCase MultiParamTypeClasses NoImplicitPrelude\n        NoMonomorphismRestriction OverloadedStrings RankNTypes\n        RecordWildCards ScopedTypeVariables StandaloneDeriving\n        TypeApplications TypeFamilies\n\n    ghc-options:        -threaded -rtsopts -O0 -with-rtsopts=-N\n    build-depends:\n        base >=4.11.1.0 && <5,\n        containers >=0.5.11.0,\n        filepath >=1.4.2,\n        hspec >=2.8.1,\n        hspec-junit-formatter -any,\n        temporary >=1.3,\n        text >=1.2.3.1,\n        xml-conduit >=1.8.0.1\n";
    }