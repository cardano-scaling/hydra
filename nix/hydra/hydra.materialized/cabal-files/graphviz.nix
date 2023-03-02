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
    flags = { test-parsing = false; };
    package = {
      specVersion = "1.12";
      identifier = { name = "graphviz"; version = "2999.20.1.0"; };
      license = "BSD-3-Clause";
      copyright = "Matthew Sackman, Ivan Lazar Miljenovic";
      maintainer = "Ivan.Miljenovic@gmail.com";
      author = "Matthew Sackman, Ivan Lazar Miljenovic";
      homepage = "https://github.com/ivan-m/graphviz";
      url = "";
      synopsis = "Bindings to Graphviz for graph visualisation.";
      description = "This library provides bindings for the Dot language used by the\nGraphviz (<http://graphviz.org/>) suite of programs for visualising\ngraphs, as well as functions to call those programs.\n\nMain features of the graphviz library include:\n\n* Almost complete coverage of all Graphviz attributes and syntax.\n\n* Support for specifying clusters.\n\n* The ability to use a custom node type.\n\n* Functions for running a Graphviz layout tool with all specified\noutput types.\n\n* The ability to not only generate but also parse Dot code with two\noptions: strict and liberal (in terms of ordering of statements).\n\n* Functions to convert FGL graphs and other graph-like data structures\nto Dot code - including support to group them into clusters - with a\nhigh degree of customisation by specifying which attributes to use\nand limited support for the inverse operation.\n\n* Round-trip support for passing an FGL graph through Graphviz to\naugment node and edge labels with positional information, etc.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
          (hsPkgs."fgl" or (errorHandler.buildDepError "fgl"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."polyparse" or (errorHandler.buildDepError "polyparse"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."colour" or (errorHandler.buildDepError "colour"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."wl-pprint-text" or (errorHandler.buildDepError "wl-pprint-text"))
          (hsPkgs."dlist" or (errorHandler.buildDepError "dlist"))
          ];
        buildable = true;
        };
      exes = {
        "graphviz-testparsing" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."graphviz" or (errorHandler.buildDepError "graphviz"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = if flags.test-parsing then true else false;
          };
        };
      tests = {
        "graphviz-testsuite" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."graphviz" or (errorHandler.buildDepError "graphviz"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."fgl" or (errorHandler.buildDepError "fgl"))
            (hsPkgs."fgl-arbitrary" or (errorHandler.buildDepError "fgl-arbitrary"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "graphviz-printparse" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."graphviz" or (errorHandler.buildDepError "graphviz"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/graphviz-2999.20.1.0.tar.gz";
      sha256 = "5ae428c3cda1eee205c9960504f490afa683a756ebbd1c710210a291d6eb1f50";
      });
    }) // {
    package-description-override = "Name:               graphviz\r\nVersion:            2999.20.1.0\r\nx-revision: 2\r\nStability:          Beta\r\nSynopsis:           Bindings to Graphviz for graph visualisation.\r\nDescription: {\r\nThis library provides bindings for the Dot language used by the\r\nGraphviz (<http://graphviz.org/>) suite of programs for visualising\r\ngraphs, as well as functions to call those programs.\r\n.\r\nMain features of the graphviz library include:\r\n.\r\n* Almost complete coverage of all Graphviz attributes and syntax.\r\n.\r\n* Support for specifying clusters.\r\n.\r\n* The ability to use a custom node type.\r\n.\r\n* Functions for running a Graphviz layout tool with all specified\r\n  output types.\r\n.\r\n* The ability to not only generate but also parse Dot code with two\r\n  options: strict and liberal (in terms of ordering of statements).\r\n.\r\n* Functions to convert FGL graphs and other graph-like data structures\r\n  to Dot code - including support to group them into clusters - with a\r\n  high degree of customisation by specifying which attributes to use\r\n  and limited support for the inverse operation.\r\n.\r\n* Round-trip support for passing an FGL graph through Graphviz to\r\n  augment node and edge labels with positional information, etc.\r\n}\r\n\r\nHomepage:           https://github.com/ivan-m/graphviz\r\nCategory:           Graphs, Graphics\r\nLicense:            BSD3\r\nLicense-File:       LICENSE.md\r\nCopyright:          Matthew Sackman, Ivan Lazar Miljenovic\r\nAuthor:             Matthew Sackman, Ivan Lazar Miljenovic\r\nMaintainer:         Ivan.Miljenovic@gmail.com\r\nBuild-Type:         Simple\r\nTested-With:        GHC == 7.6.3, GHC == 7.8.4, GHC == 7.10.2,\r\n                    GHC == 8.0.1, GHC == 8.2.2, GHC == 8.4.2,\r\n                    GHC == 8.6.2, GHC == 8.8.2, GHC == 8.10.1\r\nCabal-Version:      1.14\r\nExtra-Source-Files: TODO.md\r\n                    Changelog.md\r\n                    README.md\r\n                    FAQ.md\r\n                    utils/AttributeGenerator.hs\r\n\r\nSource-Repository head\r\n    Type:         git\r\n    Location:     https://github.com/ivan-m/graphviz.git\r\n\r\nFlag test-parsing\r\n     Description: Build a utility to test parsing of available Dot code.\r\n     Default:     False\r\n\r\nLibrary {\r\n        Default-Language:  Haskell2010\r\n\r\n        Build-Depends:     base >=4.5.0.0 && <5,\r\n                           containers,\r\n                           process,\r\n                           directory,\r\n                           temporary >=1.1 && <1.4,\r\n                           fgl >= 5.4 && < 5.9,\r\n                           filepath,\r\n                           polyparse >=1.9 && <1.14,\r\n                           bytestring >= 0.9 && < 0.12,\r\n                           colour == 2.3.*,\r\n                           mtl == 2.*,\r\n                           text,\r\n                           wl-pprint-text == 1.2.*,\r\n                           dlist >= 0.5 && < 1.1\r\n\r\n        Exposed-Modules:   Data.GraphViz\r\n                           Data.GraphViz.Types\r\n                           Data.GraphViz.Types.Canonical\r\n                           Data.GraphViz.Types.Generalised\r\n                           Data.GraphViz.Types.Graph\r\n                           Data.GraphViz.Types.Monadic\r\n                           Data.GraphViz.Parsing\r\n                           Data.GraphViz.Printing\r\n                           Data.GraphViz.Commands\r\n                           Data.GraphViz.Commands.IO\r\n                           Data.GraphViz.Attributes\r\n                           Data.GraphViz.Attributes.Complete\r\n                           Data.GraphViz.Attributes.Colors\r\n                           Data.GraphViz.Attributes.Colors.X11\r\n                           Data.GraphViz.Attributes.Colors.Brewer\r\n                           Data.GraphViz.Attributes.Colors.SVG\r\n                           Data.GraphViz.Attributes.HTML\r\n                           Data.GraphViz.PreProcessing\r\n                           Data.GraphViz.Exception\r\n                           Data.GraphViz.Algorithms\r\n\r\n                           Data.GraphViz.Attributes.Internal\r\n                           Data.GraphViz.Internal.Util\r\n                           Data.GraphViz.Internal.State\r\n                           Data.GraphViz.Types.Internal.Common\r\n\r\n        Other-Modules:     Data.GraphViz.Algorithms.Clustering\r\n                           Data.GraphViz.Attributes.Arrows\r\n                           Data.GraphViz.Attributes.ColorScheme\r\n                           Data.GraphViz.Attributes.Same\r\n                           Data.GraphViz.Attributes.Values\r\n                           Data.GraphViz.Commands.Available\r\n                           Data.GraphViz.Types.State\r\n\r\n        if True\r\n           Ghc-Options: -Wall\r\n\r\n        if impl(ghc >= 6.12.1)\r\n           Ghc-Options: -fno-warn-unused-do-bind\r\n}\r\n\r\nTest-Suite graphviz-testsuite {\r\n        Default-Language:  Haskell2010\r\n\r\n        Type:              exitcode-stdio-1.0\r\n\r\n        -- Versions controlled by library section\r\n        Build-Depends:     base,\r\n                           graphviz,\r\n                           containers,\r\n                           fgl >= 5.5.0.0,\r\n                           fgl-arbitrary == 0.2.*,\r\n                           filepath,\r\n                           hspec >= 2.1 && < 2.11,\r\n                           text,\r\n                           QuickCheck >= 2.3 && < 2.15\r\n        Build-Tool-Depends: hspec-discover:hspec-discover == 2.*\r\n\r\n        hs-Source-Dirs:    tests\r\n\r\n        Main-Is:           Main.hs\r\n\r\n\r\n        Other-Modules:       Data.GraphViz.Testing.Instances\r\n                             Data.GraphViz.Testing.Properties\r\n                             Data.GraphViz.Testing.Instances.Helpers\r\n                             Data.GraphViz.Testing.Instances.Attributes\r\n                             Data.GraphViz.Testing.Instances.Common\r\n                             Data.GraphViz.Testing.Instances.Canonical\r\n                             Data.GraphViz.Testing.Instances.Generalised\r\n                             Data.GraphViz.Testing.Instances.Graph\r\n                             Data.GraphViz.Testing.Proxy\r\n\r\n                             Data.GraphVizSpec\r\n                             Data.GraphViz.AlgorithmsSpec\r\n                             Data.GraphViz.Attributes.CompleteSpec\r\n                             Data.GraphViz.Attributes.HTMLSpec\r\n                             Data.GraphViz.PreProcessingSpec\r\n                             Data.GraphViz.Types.CanonicalSpec\r\n                             Data.GraphViz.Types.GeneralisedSpec\r\n                             Data.GraphViz.Types.GraphSpec\r\n\r\n                             Spec\r\n\r\n        if True\r\n           Ghc-Options: -Wall\r\n\r\n        if impl(ghc >= 6.12.1)\r\n           Ghc-Options: -fno-warn-unused-do-bind\r\n\r\n        GHC-Prof-Options: -rtsopts\r\n}\r\n\r\nBenchmark graphviz-printparse {\r\n        Default-Language: Haskell2010\r\n\r\n        Type:             exitcode-stdio-1.0\r\n\r\n        Build-Depends:    base,\r\n                          deepseq,\r\n                          text,\r\n                          graphviz,\r\n                          criterion >= 0.5 && < 1.7\r\n\r\n        hs-Source-Dirs:   utils\r\n\r\n        Main-Is:          Benchmark.hs\r\n\r\n        if True\r\n           Ghc-Options: -Wall\r\n\r\n        if impl(ghc >= 6.12.1)\r\n           Ghc-Options: -fno-warn-unused-do-bind\r\n\r\n        GHC-Prof-Options: -rtsopts\r\n}\r\n\r\nExecutable graphviz-testparsing {\r\n        Default-Language: Haskell2010\r\n\r\n        if flag(test-parsing)\r\n           Buildable:     True\r\n        else\r\n           Buildable:     False\r\n\r\n        hs-Source-Dirs:   utils\r\n\r\n        Main-Is:          TestParsing.hs\r\n\r\n        Build-Depends:    base,\r\n                          graphviz,\r\n                          bytestring,\r\n                          directory,\r\n                          filepath,\r\n                          text\r\n\r\n        Ghc-Options: -Wall\r\n\r\n        GHC-Prof-Options: -rtsopts\r\n}\r\n";
    }