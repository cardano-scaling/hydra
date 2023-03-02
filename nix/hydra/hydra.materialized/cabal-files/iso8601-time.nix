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
    flags = { new-time = true; };
    package = {
      specVersion = "1.8";
      identifier = { name = "iso8601-time"; version = "0.1.5"; };
      license = "MIT";
      copyright = "2013 Niklas Hambüchen <mail@nh2.me>";
      maintainer = "Niklas Hambüchen <mail@nh2.me>";
      author = "Niklas Hambüchen <mail@nh2.me>";
      homepage = "https://github.com/nh2/iso8601-time";
      url = "";
      synopsis = "Convert to/from the ISO 8601 time format";
      description = "Conversion functions between Haskell time types and the ISO 8601 format,\nwhich is often used for printing times, e.g. JavaScript's @new Date().toISOString()@.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          ] ++ (if flags.new-time
          then [ (hsPkgs."time" or (errorHandler.buildDepError "time")) ]
          else [
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."old-locale" or (errorHandler.buildDepError "old-locale"))
            ]);
        buildable = true;
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."iso8601-time" or (errorHandler.buildDepError "iso8601-time"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/iso8601-time-0.1.5.tar.gz";
      sha256 = "f2cd444b2be68402c773a4b451912817f06d33093aea691b42ebeed3630ff0c8";
      });
    }) // {
    package-description-override = "name:          iso8601-time\nversion:       0.1.5\nlicense:       MIT\ncopyright:     2013 Niklas Hambüchen <mail@nh2.me>\nauthor:        Niklas Hambüchen <mail@nh2.me>\nmaintainer:    Niklas Hambüchen <mail@nh2.me>\ncategory:      Time\nbuild-type:    Simple\nstability:     experimental\ntested-With:   GHC==7.6.3\ncabal-version: >= 1.8\nhomepage:      https://github.com/nh2/iso8601-time\nbug-Reports:   https://github.com/nh2/iso8601-time/issues\nsynopsis:      Convert to/from the ISO 8601 time format\ndescription:\n  Conversion functions between Haskell time types and the ISO 8601 format,\n  which is often used for printing times, e.g. JavaScript's @new Date().toISOString()@.\n\nsource-repository head\n  type:      git\n  location:  git://github.com/nh2/iso8601-time.git\n\nflag new-time\n  default: True\n\nlibrary\n  exposed-modules:\n    Data.Time.ISO8601\n  hs-source-dirs:\n    src\n  build-depends:\n      base             <  5\n    , time             >= 1.4\n\n  if flag(new-time)\n    build-depends:\n        time           >= 1.5\n  else\n    build-depends:\n        time           == 1.4.*\n      , old-locale     >= 1.0\n\n  ghc-options:\n    -Wall\n\n\ntest-Suite tests\n  type: exitcode-stdio-1.0\n  hs-source-dirs:\n    test\n  main-is:\n    Main.hs\n  build-depends:\n      base             >= 4 && < 5\n    , iso8601-time\n    , hspec            >= 1.3.0.1\n    , HUnit            >= 1.2\n    , time             >= 1.4\n  ghc-options:\n    -Wall\n";
    }