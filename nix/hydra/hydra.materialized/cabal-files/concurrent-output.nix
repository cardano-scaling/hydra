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
      identifier = { name = "concurrent-output"; version = "1.10.16"; };
      license = "BSD-2-Clause";
      copyright = "2015-2021 Joey Hess, 2009 Joachim Breitner";
      maintainer = "Joey Hess <id@joeyh.name>";
      author = "Joey Hess, Joachim Breitner";
      homepage = "";
      url = "";
      synopsis = "Ungarble output from several threads or commands";
      description = "Lets multiple threads and external processes concurrently output to the\nconsole, without it getting all garbled up.\n\nBuilt on top of that is a way of defining multiple output regions,\nwhich are automatically laid out on the screen and can be individually\nupdated by concurrent threads. Can be used for progress displays etc.\n\n<<https://joeyh.name/code/concurrent-output/demo2.gif>>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
          (hsPkgs."terminal-size" or (errorHandler.buildDepError "terminal-size"))
          ] ++ (pkgs.lib).optional (!system.isWindows) (hsPkgs."unix" or (errorHandler.buildDepError "unix"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/concurrent-output-1.10.16.tar.gz";
      sha256 = "b0aa45d8707f504623ed8abc67873bc139fefdb3e391e054c6adad82e7029350";
      });
    }) // {
    package-description-override = "Name: concurrent-output\nVersion: 1.10.16\nCabal-Version: >= 1.10\nLicense: BSD2\nMaintainer: Joey Hess <id@joeyh.name>\nAuthor: Joey Hess, Joachim Breitner\nStability: Stable\nCopyright: 2015-2021 Joey Hess, 2009 Joachim Breitner\nLicense-File: LICENSE\nBuild-Type: Simple\nCategory: User Interfaces\nSynopsis: Ungarble output from several threads or commands\nDescription:\n Lets multiple threads and external processes concurrently output to the\n console, without it getting all garbled up.\n .\n Built on top of that is a way of defining multiple output regions,\n which are automatically laid out on the screen and can be individually\n updated by concurrent threads. Can be used for progress displays etc.\n .\n <<https://joeyh.name/code/concurrent-output/demo2.gif>>\nExtra-Source-Files:\n  CHANGELOG\n  TODO\n  demo.hs\n  demo2.hs\n  demo3.hs\n  aptdemo.hs\n  stmdemo.hs\n\nLibrary\n  Default-Language: Haskell2010\n  GHC-Options: -Wall -fno-warn-tabs -O2\n  Build-Depends: base (>= 4.6), base < 5\n    , text (>= 0.11.0 && < 2.1)\n    , async (>= 2.0 && < 2.3)\n    , stm (>= 2.0 && < 2.6)\n    , process (>= 1.6.0 && < 1.7.0)\n    , directory (>= 1.2.0 && < 1.4.0)\n    , transformers (>= 0.3.0 && < 0.7.0)\n    , exceptions (>= 0.6.0 && < 0.11.0)\n    , ansi-terminal (>= 0.6.0 && < 0.12.0)\n    , terminal-size (>= 0.3.0 && < 0.4.0)\n  Exposed-Modules:\n    System.Console.Concurrent\n    System.Console.Concurrent.Internal\n    System.Console.Regions\n    System.Process.Concurrent\n  Other-Modules:\n    Utility.Monad\n    Utility.Exception\n\n  if (! os(Windows))\n    Build-Depends: unix (>= 2.7.0 && < 2.9.0)\n\nsource-repository head\n  type: git\n  location: git://git.joeyh.name/concurrent-output.git\n";
    }