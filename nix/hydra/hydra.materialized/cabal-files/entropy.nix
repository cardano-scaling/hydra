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
    flags = { donotgetentropy = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "entropy"; version = "0.4.1.10"; };
      license = "BSD-3-Clause";
      copyright = "Thomas DuBuisson <thomas.dubuisson@gmail.com>";
      maintainer = "Thomas DuBuisson <thomas.dubuisson@gmail.com>";
      author = "Thomas DuBuisson <thomas.dubuisson@gmail.com>";
      homepage = "https://github.com/TomMD/entropy";
      url = "";
      synopsis = "A platform independent entropy source";
      description = "A mostly platform independent method to obtain cryptographically strong entropy\n(RDRAND, urandom, CryptAPI, and patches welcome)\nUsers looking for cryptographically strong (number-theoretically\nsound) PRNGs should see the 'DRBG' package too.";
      buildType = "Custom";
      setup-depends = [
        (hsPkgs.buildPackages.Cabal or (pkgs.buildPackages.Cabal or (errorHandler.setupDepError "Cabal")))
        (hsPkgs.buildPackages.base or (pkgs.buildPackages.base or (errorHandler.setupDepError "base")))
        (hsPkgs.buildPackages.filepath or (pkgs.buildPackages.filepath or (errorHandler.setupDepError "filepath")))
        (hsPkgs.buildPackages.directory or (pkgs.buildPackages.directory or (errorHandler.setupDepError "directory")))
        (hsPkgs.buildPackages.process or (pkgs.buildPackages.process or (errorHandler.setupDepError "process")))
        ];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          ] ++ (if compiler.isGhcjs && true || system.isGhcjs
          then [
            (hsPkgs."ghcjs-dom" or (errorHandler.buildDepError "ghcjs-dom"))
            (hsPkgs."jsaddle" or (errorHandler.buildDepError "jsaddle"))
            ]
          else if system.isWindows
            then [ (hsPkgs."Win32" or (errorHandler.buildDepError "Win32")) ]
            else [ (hsPkgs."unix" or (errorHandler.buildDepError "unix")) ]);
        libs = (pkgs.lib).optionals (!(compiler.isGhcjs && true || system.isGhcjs)) ((pkgs.lib).optional (system.isWindows) (pkgs."advapi32" or (errorHandler.sysDepError "advapi32")));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/entropy-0.4.1.10.tar.gz";
      sha256 = "85ac1d53f1d1c095aedd23fdb20c8e3ada04a7fd8aa6d7e6445ae6a59a277de5";
      });
    }) // {
    package-description-override = "name:           entropy\n\nversion:        0.4.1.10\ndescription:    A mostly platform independent method to obtain cryptographically strong entropy\n                (RDRAND, urandom, CryptAPI, and patches welcome)\n                Users looking for cryptographically strong (number-theoretically\n                sound) PRNGs should see the 'DRBG' package too.\nsynopsis:       A platform independent entropy source\nlicense:        BSD3\nlicense-file:   LICENSE\ncopyright:      Thomas DuBuisson <thomas.dubuisson@gmail.com>\nauthor:         Thomas DuBuisson <thomas.dubuisson@gmail.com>\nmaintainer:     Thomas DuBuisson <thomas.dubuisson@gmail.com>\ncategory:       Data, Cryptography\nhomepage:       https://github.com/TomMD/entropy\nbug-reports:    https://github.com/TomMD/entropy/issues\nstability:      stable\n\nbuild-type:     Custom\n\n-- ^^ Test for RDRAND support using 'ghc'\ncabal-version:  >=1.10\ntested-with:    GHC == 8.2.2\n-- data-files:\nextra-source-files:   ./cbits/getrandom.c ./cbits/random_initialized.c ./cbits/rdrand.c, ./cbits/rdrand.h, README.md\n\nFlag DoNotGetEntropy\n  Description: Avoid use of the getentropy() *nix function. By default getentropy will be used\n               if detected during compilation (this plays poorly with cross compilation).\n  Default: False\n  Manual: True\n\ncustom-setup\n  setup-depends: Cabal >= 1.10 && < 3.9\n               , base < 5\n               , filepath < 1.5\n               , directory < 1.4\n               , process < 1.7\n\nlibrary\n  ghc-options:  -O2\n  exposed-modules: System.Entropy\n  if impl(ghcjs) || os(ghcjs)\n    other-modules: System.EntropyGhcjs\n  else {\n    if os(windows)\n      other-modules: System.EntropyWindows\n    else {\n      other-modules: System.EntropyNix\n    }\n  }\n  other-extensions:    CPP, ForeignFunctionInterface, BangPatterns,\n                       ScopedTypeVariables\n  build-depends:       base >= 4.8 && < 5, bytestring\n\n  default-language:    Haskell2010\n\n  if impl(ghcjs) || os(ghcjs) {\n    build-depends:     ghcjs-dom >= 0.9.5.0 && < 1\n                     , jsaddle\n  }\n  else {\n    if arch(x86_64)\n      cpp-options: -Darch_x86_64\n      cc-options:  -Darch_x86_64 -O2\n      -- gcc 4.8.2 on i386 fails to compile rdrand.c when using -fPIC!\n      c-sources:    cbits/rdrand.c\n      include-dirs: cbits\n    if arch(i386)\n      cpp-options: -Darch_i386\n      cc-options:  -Darch_i386 -O2\n    if os(windows)\n      build-depends: Win32 >= 2.5\n      cpp-options: -DisWindows\n      cc-options:  -DisWindows\n      extra-libraries: advapi32\n    else\n      Build-Depends: unix\n      c-sources: cbits/getrandom.c cbits/random_initialized.c\n  }\n  if flag(DoNotGetEntropy) {\n    cc-options: -DDO_NOT_USE_GET_ENTROPY\n  }\n\n\nsource-repository head\n    type:       git\n    location:   https://github.com/TomMD/entropy\n";
    }