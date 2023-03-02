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
      specVersion = "1.8";
      identifier = { name = "formatting"; version = "6.3.7"; };
      license = "BSD-3-Clause";
      copyright = "2013 Chris Done, Shachaf Ben-Kiki, Martijn van Steenbergen, Mike Meyer, 2011 MailRank, Inc.";
      maintainer = "chrisdone@gmail.com";
      author = "Chris Done, Shachaf Ben-Kiki, Martijn van Steenbergen, Mike Meyer, Bryan O'Sullivan";
      homepage = "";
      url = "";
      synopsis = "Combinator-based type-safe formatting (like printf() or FORMAT)";
      description = "Combinator-based type-safe formatting (like printf() or FORMAT), modelled from the HoleyMonoids package.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."integer-gmp" or (errorHandler.buildDepError "integer-gmp"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."old-locale" or (errorHandler.buildDepError "old-locale"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."clock" or (errorHandler.buildDepError "clock"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."integer-gmp" or (errorHandler.buildDepError "integer-gmp"))
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          ];
        buildable = true;
        };
      tests = {
        "formatting-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."formatting" or (errorHandler.buildDepError "formatting"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/formatting-6.3.7.tar.gz";
      sha256 = "0cdb4fc2c33612db08cd07926ac4fbea6b9f3c31955ed7d212ae04586f585c1a";
      });
    }) // {
    package-description-override = "name:                formatting\nversion:             6.3.7\nx-revision: 2\nsynopsis:            Combinator-based type-safe formatting (like printf() or FORMAT)\ndescription:         Combinator-based type-safe formatting (like printf() or FORMAT), modelled from the HoleyMonoids package.\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              Chris Done, Shachaf Ben-Kiki, Martijn van Steenbergen, Mike Meyer, Bryan O'Sullivan\nmaintainer:          chrisdone@gmail.com\ncopyright:           2013 Chris Done, Shachaf Ben-Kiki, Martijn van Steenbergen, Mike Meyer, 2011 MailRank, Inc.\ncategory:            Text\nbuild-type:          Simple\ncabal-version:       >=1.8\nextra-source-files:  CHANGELOG.md\n\nlibrary\n  build-depends: integer-gmp <1.1\n\n  exposed-modules:\n    Formatting\n    Formatting.Formatters\n    Formatting.ShortFormatters\n    Formatting.Examples\n    Formatting.Time\n    Formatting.Clock\n    Formatting.Internal\n    Formatting.Internal.Raw\n    Formatting.Buildable\n\n  other-modules:\n    Data.Text.Format.Functions\n    Data.Text.Format.Types\n    Data.Text.Format\n    Data.Text.Format.Int\n\n  build-depends:\n    base >= 4.5 && < 5,\n    text >= 0.11.0.8,\n    time,\n    old-locale,\n    scientific >= 0.3.0.0,\n    clock >= 0.4,\n    array,\n    ghc-prim,\n    text >= 0.11.0.8,\n    transformers,\n    bytestring >=0.10.4,\n    integer-gmp >= 0.2,\n    semigroups\n\n  hs-source-dirs:    src\n  ghc-options:       -O2\n  cpp-options: -DINTEGER_GMP\n\ntest-suite formatting-test\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      test\n  main-is:             Spec.hs\n  build-depends:       base, formatting, hspec, semigroups, text\n  ghc-options:         -Wall -threaded -rtsopts -with-rtsopts=-N\n\nsource-repository head\n  type:     git\n  location: http://github.com/chrisdone/formatting\n";
    }