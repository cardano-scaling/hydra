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
    flags = { split-base = true; cli = false; };
    package = {
      specVersion = "1.6";
      identifier = { name = "wcwidth"; version = "0.0.2"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "oss@solidsnack.be";
      author = "Jason Dusek";
      homepage = "http://github.com/solidsnack/wcwidth/";
      url = "";
      synopsis = "Native wcwidth.";
      description = "Bindings for your system's native wcwidth and a command line tool to examine\nthe widths assigned by it. The command line tool can compile a width table\nto Haskell code that assigns widths to the Char type.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          ] ++ [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
        };
      exes = {
        "wcwidth-tools" = {
          depends = [
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."setlocale" or (errorHandler.buildDepError "setlocale"))
            (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
            (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
            ] ++ [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
          buildable = if flags.cli then true else false;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/wcwidth-0.0.2.tar.gz";
      sha256 = "ffc68736a3bbde3e8157710f29f4a99c0ca593c41194579c54a92c62f6c12ed8";
      });
    }) // {
    package-description-override = "name                          : wcwidth\nversion                       : 0.0.2\ncategory                      : Text\nlicense                       : BSD3\nlicense-file                  : LICENSE\nauthor                        : Jason Dusek\nmaintainer                    : oss@solidsnack.be\nhomepage                      : http://github.com/solidsnack/wcwidth/\nsynopsis                      : Native wcwidth.\ndescription                   :\n  Bindings for your system's native wcwidth and a command line tool to examine\n  the widths assigned by it. The command line tool can compile a width table\n  to Haskell code that assigns widths to the Char type.\n\n\ncabal-version                 : >= 1.6.0\nbuild-type                    : Simple\nextra-source-files            : CompileRanges.hs\n\n\nflag split-base\n\nflag cli\n  description                 : Enable command line tool.\n  default                     : False\n\n\nlibrary\n  if flag(split-base)\n    build-depends             : base >= 4 && < 5\n  else\n    build-depends             : base < 4\n  build-depends               : containers\n  exposed-modules             : Data.Char.WCWidth\n  extensions                  : StandaloneDeriving\n                                ForeignFunctionInterface\n                                OverloadedStrings\n\n\nexecutable                      wcwidth-tools\n  main-is                     : WCWidthTableaux.hs\n  if flag(cli)\n    buildable                 : True\n  else\n    buildable                 : False\n  if flag(split-base)\n    build-depends             : base >= 4 && < 5\n  else\n    build-depends             : base < 4\n  build-depends               : containers\n                              , bytestring\n                              , setlocale >= 0.0.3\n                              , utf8-string >= 0.3\n                              , attoparsec >= 0.8.5\n  extensions                  : StandaloneDeriving\n                                ForeignFunctionInterface\n                                OverloadedStrings\n\n\n";
    }