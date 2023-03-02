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
      identifier = { name = "xml-types"; version = "0.3.8"; };
      license = "MIT";
      copyright = "";
      maintainer = "Stephen Paul Weber <singpolyma@singpolyma.net>";
      author = "John Millikin <jmillikin@gmail.com>";
      homepage = "https://git.singpolyma.net/xml-types-haskell";
      url = "";
      synopsis = "Basic types for representing XML";
      description = "Basic types for representing XML.\n\nThe idea is to have a full set of appropriate types, which various XML\nlibraries can share. Instead of having equivalent-but-incompatible types\nfor every binding, parser, or client, they all share the same types can\ncan thus interoperate easily.\n\nThis library contains complete types for most parts of an XML document,\nincluding the prologue, node tree, and doctype. Some basic combinators\nare included for common tasks, including traversing the node tree and\nfiltering children.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/xml-types-0.3.8.tar.gz";
      sha256 = "dad5e4ce602b7d1f4be37c0cfd99a261a4573746bfd80d917dc955b72da84c80";
      });
    }) // {
    package-description-override = "name: xml-types\nversion: 0.3.8\nsynopsis: Basic types for representing XML\nlicense: MIT\nlicense-file: COPYING\nauthor: John Millikin <jmillikin@gmail.com>\nmaintainer: Stephen Paul Weber <singpolyma@singpolyma.net>\nbuild-type: Simple\ncabal-version: >= 1.10\ncategory: Text, XML\nstability: experimental\nhomepage: https://git.singpolyma.net/xml-types-haskell\nbug-reports: mailto:dev@singpolyma.net\ndescription:\n   Basic types for representing XML.\n   .\n   The idea is to have a full set of appropriate types, which various XML\n   libraries can share. Instead of having equivalent-but-incompatible types\n   for every binding, parser, or client, they all share the same types can\n   can thus interoperate easily.\n   .\n   This library contains complete types for most parts of an XML document,\n   including the prologue, node tree, and doctype. Some basic combinators\n   are included for common tasks, including traversing the node tree and\n   filtering children.\n\nsource-repository head\n  type: git\n  location: https://git.singpolyma.net/xml-types-haskell\n\nsource-repository this\n  type: git\n  location: https://git.singpolyma.net/xml-types-haskell\n  tag: 0.3.8\n\nlibrary\n  default-language: Haskell2010\n  ghc-options: -Wall\n  hs-source-dirs: lib\n\n  build-depends:\n      base >= 3.0 && < 5.0\n    , deepseq >= 1.1.0.0\n    , text\n\n  exposed-modules:\n    Data.XML.Types\n";
    }