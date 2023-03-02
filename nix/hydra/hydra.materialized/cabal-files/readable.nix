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
      specVersion = "1.6";
      identifier = { name = "readable"; version = "0.3.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "mightybyte@gmail.com";
      author = "Doug Beardsley";
      homepage = "https://github.com/mightybyte/readable";
      url = "";
      synopsis = "Reading from Text and ByteString";
      description = "Provides a Readable type class for reading data types from ByteString and\nText.  Also includes efficient implementations for common data types.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/readable-0.3.1.tar.gz";
      sha256 = "703037ad2cca4d6d42ba23e2758d1911cd82e3e922c4078076c273231e4b43c9";
      });
    }) // {
    package-description-override = "name:           readable\nversion:        0.3.1\nsynopsis:       Reading from Text and ByteString\n\ndescription:\n  Provides a Readable type class for reading data types from ByteString and\n  Text.  Also includes efficient implementations for common data types.\n\nlicense:        BSD3\nlicense-file:   LICENSE\nauthor:         Doug Beardsley\nmaintainer:     mightybyte@gmail.com\nbuild-type:     Simple\ncabal-version:  >= 1.6\nhomepage:       https://github.com/mightybyte/readable\ncategory:       Text\n\nextra-source-files:\n  CHANGELOG.md\n  LICENSE,\n  README.md,\n  Setup.hs\n\nLibrary\n  hs-source-dirs: src\n\n  exposed-modules:\n    Data.Readable\n\n  build-depends:\n    base        >= 4    && < 5,\n    bytestring  >= 0.9  && < 0.11,\n    text        >= 0.11 && < 1.3\n\n  ghc-prof-options: -prof -auto-all\n  ghc-options: -Wall -fwarn-tabs\n\nsource-repository head\n  type:     git\n  location: git://github.com/mightybyte/readable.git\n";
    }