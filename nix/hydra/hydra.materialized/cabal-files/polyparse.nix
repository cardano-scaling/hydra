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
      identifier = { name = "polyparse"; version = "1.13"; };
      license = "LicenseRef-LGPL";
      copyright = "(c) 2006-2016 Malcolm Wallace";
      maintainer = "author";
      author = "Malcolm Wallace <Malcolm.Wallace@me.com>";
      homepage = "http://code.haskell.org/~malcolm/polyparse/";
      url = "";
      synopsis = "A variety of alternative parser combinator libraries.";
      description = "This version, 1.13 is a Non-Maintainer Upload (NMU).\nReport issues to the Hackage Trustees issue tracker.\n\nA variety of alternative parser combinator libraries, including\nthe original HuttonMeijer set.  The Poly sets have features like\ngood error reporting, arbitrary token type, running state, lazy\nparsing, and so on.  Finally, Text.Parse is a proposed\nreplacement for the standard Read class, for better\ndeserialisation of Haskell values from Strings.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."fail" or (errorHandler.buildDepError "fail"))) ++ (pkgs.lib).optionals (compiler.isGhc && true) [
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/polyparse-1.13.tar.gz";
      sha256 = "1c4c72980e1e5a4f07fea65ca08b2399581d2a6aa21eb1078f7ad286c279707b";
      });
    }) // {
    package-description-override = "name:           polyparse\nversion:        1.13\nx-revision:     5\nlicense:        LGPL\nlicense-files:   COPYRIGHT, LICENCE-LGPL, LICENCE-commercial\ncopyright:      (c) 2006-2016 Malcolm Wallace\nauthor:         Malcolm Wallace <Malcolm.Wallace@me.com>\nmaintainer:     author\nhomepage:       http://code.haskell.org/~malcolm/polyparse/\nbug-reports:    https://github.com/haskell-infra/hackage-trustees/issues\ncategory:       Text, Parsing\nsynopsis:       A variety of alternative parser combinator libraries.\ndescription:\n        This version, 1.13 is a Non-Maintainer Upload (NMU).\n        Report issues to the Hackage Trustees issue tracker.\n        .\n        A variety of alternative parser combinator libraries, including\n        the original HuttonMeijer set.  The Poly sets have features like\n        good error reporting, arbitrary token type, running state, lazy\n        parsing, and so on.  Finally, Text.Parse is a proposed\n        replacement for the standard Read class, for better\n        deserialisation of Haskell values from Strings.\nbuild-type:     Simple\ncabal-version:  >=1.8\nextra-source-files: Changelog.md\n\ntested-with:\n  GHC ==9.4.1\n   || ==9.2.4\n   || ==9.0.2\n   || ==8.10.7\n   || ==8.8.4\n   || ==8.6.5\n   || ==8.4.4\n   || ==8.2.2\n   || ==8.0.2\n   || ==7.10.3\n   || ==7.8.4\n   || ==7.6.3\n   || ==7.4.2\n   || ==7.2.2\n   || ==7.0.4\n\nsource-repository head\n  type:     darcs\n  location: http://code.haskell.org/polyparse\n\nsource-repository this\n  type:      git\n  location:  https://github.com/hackage-trustees/malcolm-wallace-universe.git\n  tag:       1.12.1\n\nlibrary\n  hs-source-dirs:       src\n  build-depends:        base >= 4.3.1.0 && < 4.18\n\n  if !impl(ghc >= 8.0)\n     build-depends: fail == 4.9.*\n\n  exposed-modules:\n        Text.ParserCombinators.HuttonMeijer,\n        Text.ParserCombinators.HuttonMeijerWallace,\n        Text.ParserCombinators.Poly,\n        Text.ParserCombinators.Poly.Base,\n        Text.ParserCombinators.Poly.Result,\n        Text.ParserCombinators.Poly.Parser,\n        Text.ParserCombinators.Poly.Plain,\n        Text.ParserCombinators.Poly.Lazy,\n        Text.ParserCombinators.Poly.StateParser,\n        Text.ParserCombinators.Poly.State,\n        Text.ParserCombinators.Poly.StateLazy,\n        Text.ParserCombinators.Poly.Lex,\n        Text.Parse\n  if impl(ghc)\n    build-depends:      bytestring >= 0.9.1.0 && < 0.12\n    build-depends:      text >= 1.2.3.0 && <1.3 || >=2.0 && <2.1\n    exposed-modules:\n        Text.ParserCombinators.Poly.ByteString\n        Text.ParserCombinators.Poly.ByteStringChar\n        Text.Parse.ByteString\n        Text.ParserCombinators.Poly.Text\n        Text.ParserCombinators.Poly.StateText\n--      Text.Parse.Text\n  cpp-options:          -DVERSION=\"1.12\"\n  nhc98-options:        -K6M\n  extensions:           CPP\n";
    }