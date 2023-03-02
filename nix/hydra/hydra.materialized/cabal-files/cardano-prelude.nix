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
    flags = { development = false; };
    package = {
      specVersion = "2.2";
      identifier = { name = "cardano-prelude"; version = "0.1.0.0"; };
      license = "MIT";
      copyright = "2018-2021 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "A Prelude replacement for the Cardano project";
      description = "A Prelude replacement for the Cardano project";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."canonical-json" or (errorHandler.buildDepError "canonical-json"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."formatting" or (errorHandler.buildDepError "formatting"))
          (hsPkgs."ghc-heap" or (errorHandler.buildDepError "ghc-heap"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."integer-gmp" or (errorHandler.buildDepError "integer-gmp"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."protolude" or (errorHandler.buildDepError "protolude"))
          (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/cardano-prelude-0.1.0.0.tar.gz";
      sha256 = "5ac44c09345f876473416c92a24a25924bcbb6efb90b76b3e62c688dabed6f29";
      });
    }) // {
    package-description-override = "cabal-version: 2.2\n\nname:                 cardano-prelude\nversion:              0.1.0.0\nsynopsis:             A Prelude replacement for the Cardano project\ndescription:          A Prelude replacement for the Cardano project\nlicense:              MIT\nlicense-file:         LICENSE\nauthor:               IOHK\nmaintainer:           operations@iohk.io\ncopyright:            2018-2021 IOHK\ncategory:             Currency\nbuild-type:           Simple\nextra-source-files:   ChangeLog.md, README.md cbits/hashset.h cbits/worklist.h\n\nflag development\n  description: Disable `-Werror`\n  default: False\n  manual: True\n\nlibrary\n  hs-source-dirs:     src\n  exposed-modules:    Cardano.Prelude\n                      Data.Semigroup.Action\n  other-modules:      Cardano.Prelude.Base\n                      Cardano.Prelude.Error\n                      Cardano.Prelude.Formatting\n                      Cardano.Prelude.GHC.Heap\n                      Cardano.Prelude.GHC.Heap.NormalForm\n                      Cardano.Prelude.GHC.Heap.Size\n                      Cardano.Prelude.GHC.Heap.Tree\n                      Cardano.Prelude.HeapWords\n                      Cardano.Prelude.Json.Canonical\n                      Cardano.Prelude.Json.Parse\n                      Cardano.Prelude.Orphans\n                      Cardano.Prelude.Strict\n\n  build-depends:      base               >= 4.14       && < 4.15\n                    , aeson\n                    , array\n                    , base16-bytestring  >= 1\n                    , bytestring\n                    , canonical-json\n                    , cborg\n                    , containers\n                    , formatting\n                    , ghc-heap\n                    , ghc-prim\n                    , integer-gmp\n                    , mtl\n                    , protolude\n                    , tagged\n                    , text\n                    , time\n                    , vector\n  default-language:   Haskell2010\n  default-extensions: NoImplicitPrelude\n  c-sources:          cbits/hashset.c\n                      cbits/worklist.c\n                      cbits/closure_size.c\n  ghc-options:        -Weverything\n                      -fno-warn-all-missed-specialisations\n                      -fno-warn-missing-deriving-strategies\n                      -fno-warn-missing-import-lists\n                      -fno-warn-missing-safe-haskell-mode\n                      -fno-warn-prepositive-qualified-module\n                      -fno-warn-safe\n                      -fno-warn-unsafe\n  cc-options:         -Wall\n\n  if (!flag(development))\n    ghc-options:      -Werror\n";
    }