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
      identifier = { name = "fin"; version = "0.1.1"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2017-2019 Oleg Grenrus";
      maintainer = "Oleg.Grenrus <oleg.grenrus@iki.fi>";
      author = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      homepage = "https://github.com/phadej/vec";
      url = "";
      synopsis = "Nat and Fin: peano naturals and finite numbers";
      description = "This package provides two simple types, and some tools to work with them.\nAlso on type level as @DataKinds@.\n\n@\n\\-- Peano naturals\ndata Nat = Z | S Nat\n\n\\-- Finite naturals\ndata Fin (n :: Nat) where\n\\    Z :: Fin ('S n)\n\\    S :: Fin n -> Fin ('Nat.S n)\n@\n\n[vec](https://hackage.haskell.org/package/vec) implements length-indexed\n(sized) lists using this package for indexes.\n\nThe \"Data.Fin.Enum\" module let's work generically with enumerations.\n\nSee [Hasochism: the pleasure and pain of dependently typed haskell programming](https://doi.org/10.1145/2503778.2503786)\nby Sam Lindley and Conor McBride for answers to /how/ and /why/.\nRead [APLicative Programming with Naperian Functors](https://doi.org/10.1007/978-3-662-54434-1_21)\nby Jeremy Gibbons for (not so) different ones.\n\n=== Similar packages\n\n* [finite-typelits](https://hackage.haskell.org/package/finite-typelits)\n. Is a great package, but uses @GHC.TypeLits@.\n\n* [type-natural](https://hackage.haskell.org/package/type-natural) depends\non @singletons@ package. @fin@ will try to stay light on the dependencies,\nand support as many GHC versions as practical.\n\n* [peano](https://hackage.haskell.org/package/peano) is very incomplete\n\n* [nat](https://hackage.haskell.org/package/nat) as well.\n\n* [PeanoWitnesses](https://hackage.haskell.org/package/PeanoWitnesses)\ndoesn't use @DataKinds@.\n\n* [type-combinators](https://hackage.haskell.org/package/type-combinators)\nis big package too.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = (([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."dec" or (errorHandler.buildDepError "dec"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.2")) (hsPkgs."bifunctors" or (errorHandler.buildDepError "bifunctors"))) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "7.10")) [
          (hsPkgs."nats" or (errorHandler.buildDepError "nats"))
          (hsPkgs."void" or (errorHandler.buildDepError "void"))
          ];
        buildable = true;
        };
      tests = {
        "inspection" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."fin" or (errorHandler.buildDepError "fin"))
            (hsPkgs."inspection-testing" or (errorHandler.buildDepError "inspection-testing"))
            (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
            ];
          buildable = if !(compiler.isGhc && (compiler.version).ge "8.0")
            then false
            else true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/fin-0.1.1.tar.gz";
      sha256 = "31e3174c0220ed6ca07c02982105279d01309e71791534355c612e1a45478c7f";
      });
    }) // {
    package-description-override = "cabal-version:      >=1.10\nname:               fin\nversion:            0.1.1\nx-revision:         2\nsynopsis:           Nat and Fin: peano naturals and finite numbers\ncategory:           Data, Dependent Types, Singletons, Math\ndescription:\n  This package provides two simple types, and some tools to work with them.\n  Also on type level as @DataKinds@.\n  .\n  @\n  \\-- Peano naturals\n  data Nat = Z | S Nat\n  .\n  \\-- Finite naturals\n  data Fin (n :: Nat) where\n  \\    Z :: Fin ('S n)\n  \\    S :: Fin n -> Fin ('Nat.S n)\n  @\n  .\n  [vec](https://hackage.haskell.org/package/vec) implements length-indexed\n  (sized) lists using this package for indexes.\n  .\n  The \"Data.Fin.Enum\" module let's work generically with enumerations.\n  .\n  See [Hasochism: the pleasure and pain of dependently typed haskell programming](https://doi.org/10.1145/2503778.2503786)\n  by Sam Lindley and Conor McBride for answers to /how/ and /why/.\n  Read [APLicative Programming with Naperian Functors](https://doi.org/10.1007/978-3-662-54434-1_21)\n  by Jeremy Gibbons for (not so) different ones.\n  .\n  === Similar packages\n  .\n  * [finite-typelits](https://hackage.haskell.org/package/finite-typelits)\n  . Is a great package, but uses @GHC.TypeLits@.\n  .\n  * [type-natural](https://hackage.haskell.org/package/type-natural) depends\n  on @singletons@ package. @fin@ will try to stay light on the dependencies,\n  and support as many GHC versions as practical.\n  .\n  * [peano](https://hackage.haskell.org/package/peano) is very incomplete\n  .\n  * [nat](https://hackage.haskell.org/package/nat) as well.\n  .\n  * [PeanoWitnesses](https://hackage.haskell.org/package/PeanoWitnesses)\n  doesn't use @DataKinds@.\n  .\n  * [type-combinators](https://hackage.haskell.org/package/type-combinators)\n  is big package too.\n\nhomepage:           https://github.com/phadej/vec\nbug-reports:        https://github.com/phadej/vec/issues\nlicense:            BSD3\nlicense-file:       LICENSE\nauthor:             Oleg Grenrus <oleg.grenrus@iki.fi>\nmaintainer:         Oleg.Grenrus <oleg.grenrus@iki.fi>\ncopyright:          (c) 2017-2019 Oleg Grenrus\nbuild-type:         Simple\nextra-source-files: ChangeLog.md\ntested-with:\n  GHC ==7.8.4\n   || ==7.10.3\n   || ==8.0.2\n   || ==8.2.2\n   || ==8.4.4\n   || ==8.6.5\n   || ==8.8.3\n   || ==8.10.1\n\nsource-repository head\n  type:     git\n  location: https://github.com/phadej/vec.git\n  subdir:   fin\n\nlibrary\n  exposed-modules:\n    Data.Fin\n    Data.Fin.Enum\n    Data.Nat\n    Data.Type.Nat\n    Data.Type.Nat.LE\n    Data.Type.Nat.LE.ReflStep\n    Data.Type.Nat.LT\n\n  build-depends:\n      base        >=4.7     && <4.15\n    , dec         >=0.0.3   && <0.1\n    , deepseq     >=1.3.0.2 && <1.5\n    , hashable    >=1.2.7.0 && <1.4\n    , QuickCheck  >=2.13.2  && <2.15\n\n  if !impl(ghc >=8.2)\n    build-depends: bifunctors >=5.5.3 && <5.6\n\n  if !impl(ghc >=8.0)\n    build-depends: semigroups >=0.18.4 && <0.20\n\n  if !impl(ghc >=7.10)\n    build-depends:\n        nats  >=1.1.2 && <1.2\n      , void  >=0.7.2 && <0.8\n\n  ghc-options:      -Wall -fprint-explicit-kinds\n  hs-source-dirs:   src\n  default-language: Haskell2010\n\n-- dump-core\n-- if impl(ghc >= 8.0)\n--  build-depends: dump-core\n--  ghc-options: -fplugin=DumpCore -fplugin-opt DumpCore:core-html\n\ntest-suite inspection\n  type:             exitcode-stdio-1.0\n  main-is:          Inspection.hs\n  ghc-options:      -Wall -fprint-explicit-kinds\n  hs-source-dirs:   test\n  default-language: Haskell2010\n  build-depends:\n      base\n    , fin\n    , inspection-testing  >=0.2.0.1 && <0.5\n    , tagged\n\n  if !impl(ghc >=8.0)\n    buildable: False\n";
    }