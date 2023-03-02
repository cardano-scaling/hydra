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
      specVersion = "1.18";
      identifier = { name = "composition-prelude"; version = "3.0.0.2"; };
      license = "BSD-3-Clause";
      copyright = "Copyright: (c) 2017-2020 Vanessa McHale";
      maintainer = "vamchale@gmail.com";
      author = "Vanessa McHale";
      homepage = "";
      url = "";
      synopsis = "Higher-order function combinators";
      description = "Replacement for [composition](hackage.haskell.org/package/composition) or [composition-extra](hackage.haskell.org/package/composition-extra), exporting everything in one module.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/composition-prelude-3.0.0.2.tar.gz";
      sha256 = "050cfc9738a0dc727570aff513c34accf78da0d4941aa695bd26d54126b53194";
      });
    }) // {
    package-description-override = "cabal-version:   1.18\nname:            composition-prelude\nversion:         3.0.0.2\nlicense:         BSD3\nlicense-file:    LICENSE\ncopyright:       Copyright: (c) 2017-2020 Vanessa McHale\nmaintainer:      vamchale@gmail.com\nauthor:          Vanessa McHale\nbug-reports:     https://hub.darcs.net/vmchale/composition-prelude/issues\nsynopsis:        Higher-order function combinators\ndescription:\n    Replacement for [composition](hackage.haskell.org/package/composition) or [composition-extra](hackage.haskell.org/package/composition-extra), exporting everything in one module.\n\ncategory:        Control, Data\nbuild-type:      Simple\nextra-doc-files:\n    README.md\n    CHANGELOG.md\n\nsource-repository head\n    type:     darcs\n    location: https://hub.darcs.net/vmchale/composition-prelude\n\nflag development\n    description: Enable `-Werror`\n    default:     False\n    manual:      True\n\nlibrary\n    exposed-modules:  Control.Composition\n    hs-source-dirs:   src\n    default-language: Haskell98\n    ghc-options:      -Wall\n    build-depends:    base >=4.11 && <5\n\n    if flag(development)\n        ghc-options: -Werror\n\n    if impl(ghc >=8.0)\n        ghc-options: -Wincomplete-uni-patterns -Wincomplete-record-updates\n";
    }