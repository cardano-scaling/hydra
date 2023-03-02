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
      identifier = { name = "code-page"; version = "0.2.1"; };
      license = "BSD-3-Clause";
      copyright = "(C) 2016-2017 Ryan Scott";
      maintainer = "Ryan Scott <ryan.gl.scott@gmail.com>";
      author = "Ryan Scott";
      homepage = "https://github.com/RyanGlScott/code-page";
      url = "";
      synopsis = "Windows code page library for Haskell";
      description = "This library provides two modules:\n\n* \"System.IO.CodePage\": a cross-platform module that exports\nfunctions which adjust code pages on Windows, and do nothing\non other operating systems.\n\n* \"System.Win32.CodePage\": On Windows, this exports functions\nfor getting, setting, and analyzing code pages. On other\noperating systems, this module exports nothing.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ] ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"));
        build-tools = [
          (hsPkgs.buildPackages.hsc2hs.components.exes.hsc2hs or (pkgs.buildPackages.hsc2hs or (errorHandler.buildToolDepError "hsc2hs:hsc2hs")))
          ];
        buildable = true;
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."code-page" or (errorHandler.buildDepError "code-page"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/code-page-0.2.1.tar.gz";
      sha256 = "b2f90e19c61ed8a6ff7295f7f123d4a9913c790d4cf2c6029bc299293fdb2aaa";
      });
    }) // {
    package-description-override = "name:                code-page\nversion:             0.2.1\nsynopsis:            Windows code page library for Haskell\ndescription:         This library provides two modules:\n                     .\n                     * \"System.IO.CodePage\": a cross-platform module that exports\n                     functions which adjust code pages on Windows, and do nothing\n                     on other operating systems.\n                     .\n                     * \"System.Win32.CodePage\": On Windows, this exports functions\n                     for getting, setting, and analyzing code pages. On other\n                     operating systems, this module exports nothing.\nhomepage:            https://github.com/RyanGlScott/code-page\nbug-reports:         https://github.com/RyanGlScott/code-page/issues\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              Ryan Scott\nmaintainer:          Ryan Scott <ryan.gl.scott@gmail.com>\nstability:           Provisional\ncopyright:           (C) 2016-2017 Ryan Scott\ncategory:            System\nbuild-type:          Simple\nextra-source-files:  CHANGELOG.md, README.md, include/*.h\ncabal-version:       >=1.10\ntested-with:         GHC == 7.0.4\n                   , GHC == 7.2.2\n                   , GHC == 7.4.2\n                   , GHC == 7.6.3\n                   , GHC == 7.8.4\n                   , GHC == 7.10.3\n                   , GHC == 8.0.2\n                   , GHC == 8.2.2\n                   , GHC == 8.4.4\n                   , GHC == 8.6.5\n                   , GHC == 8.8.4\n                   , GHC == 8.10.2\n\nsource-repository head\n  type:                git\n  location:            https://github.com/RyanGlScott/code-page\n\nlibrary\n  exposed-modules:     System.IO.CodePage\n                       System.IO.CodePage.Internal\n                       System.Win32.CodePage\n  build-depends:       base >= 4.3 && < 5\n  build-tools:         hsc2hs\n  if os(windows)\n    include-dirs:      include\n    includes:          windows_cconv.h\n    cpp-options:       \"-DWINDOWS\"\n    build-depends:     Win32\n\n  hs-source-dirs:      src\n  default-language:    Haskell2010\n  ghc-options:         -Wall\n\ntest-suite tests\n  type:                exitcode-stdio-1.0\n  main-is:             Tests.hs\n\n  build-depends:       base >= 4.3 && < 5\n                     , code-page\n\n  hs-source-dirs:      tests\n  default-language:    Haskell2010\n  ghc-options:         -Wall -threaded -rtsopts\n";
    }