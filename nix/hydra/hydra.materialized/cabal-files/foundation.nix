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
    flags = {
      experimental = false;
      bench-all = false;
      minimal-deps = false;
      bounds-check = false;
      doctest = false;
      linktest = false;
      };
    package = {
      specVersion = "1.18";
      identifier = { name = "foundation"; version = "0.0.29"; };
      license = "BSD-3-Clause";
      copyright = "2015-2017 Vincent Hanquez <vincent@snarc.org>, 2017- Foundation Maintainers";
      maintainer = "vincent@snarc.org";
      author = "Vincent Hanquez <vincent@snarc.org>";
      homepage = "https://github.com/haskell-foundation/foundation";
      url = "";
      synopsis = "Alternative prelude with batteries and no dependencies";
      description = "A custom prelude with no dependencies apart from base.\n\nThis package has the following goals:\n\n* provide a base like sets of modules that provide a consistent set of features and bugfixes across multiple versions of GHC (unlike base).\n\n* provide a better and more efficient prelude than base's prelude.\n\n* be self-sufficient: no external dependencies apart from base.\n\n* provide better data-types: packed unicode string by default, arrays.\n\n* Better numerical classes that better represent mathematical thing (No more all-in-one Num).\n\n* Better I/O system with less Lazy IO\n\n* Usual partial functions distinguished through type system";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."basement" or (errorHandler.buildDepError "basement"))
          ] ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).lt "8.10" || compiler.isGhcjs && true)) ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          ] ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."Win32" or (errorHandler.buildDepError "Win32")));
        libs = (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).lt "8.10" || compiler.isGhcjs && true)) ((pkgs.lib).optionals (system.isWindows) ((pkgs.lib).optional (system.isI386) (pkgs."gcc" or (errorHandler.sysDepError "gcc"))));
        buildable = if compiler.isGhc && (compiler.version).lt "8.10" || compiler.isGhcjs && true
          then false
          else true;
        };
      tests = {
        "check-foundation" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."basement" or (errorHandler.buildDepError "basement"))
            (hsPkgs."foundation" or (errorHandler.buildDepError "foundation"))
            ];
          buildable = true;
          };
        "foundation-link" = {
          depends = (pkgs.lib).optionals (flags.linktest) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."foundation" or (errorHandler.buildDepError "foundation"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            ];
          buildable = if flags.linktest then true else false;
          };
        "doctest" = {
          depends = (pkgs.lib).optionals (!flags.minimal-deps) ((pkgs.lib).optionals (flags.doctest) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
            ]);
          buildable = if flags.minimal-deps
            then false
            else if flags.doctest then true else false;
          };
        };
      benchmarks = {
        "bench" = {
          depends = (pkgs.lib).optionals (!(flags.minimal-deps || compiler.isGhc && (compiler.version).lt "7.10")) ([
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."gauge" or (errorHandler.buildDepError "gauge"))
            (hsPkgs."basement" or (errorHandler.buildDepError "basement"))
            (hsPkgs."foundation" or (errorHandler.buildDepError "foundation"))
            ] ++ (pkgs.lib).optionals (flags.bench-all) [
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            ]);
          buildable = if flags.minimal-deps || compiler.isGhc && (compiler.version).lt "7.10"
            then false
            else true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/foundation-0.0.29.tar.gz";
      sha256 = "34839bab424ffd5aed228271d75b15a84ed94eab515e44be909a9b37948173c1";
      });
    }) // {
    package-description-override = "name:                foundation\nversion:             0.0.29\nsynopsis:            Alternative prelude with batteries and no dependencies\ndescription:\n    A custom prelude with no dependencies apart from base.\n    .\n    This package has the following goals:\n    .\n    * provide a base like sets of modules that provide a consistent set of features and bugfixes across multiple versions of GHC (unlike base).\n    .\n    * provide a better and more efficient prelude than base's prelude.\n    .\n    * be self-sufficient: no external dependencies apart from base.\n    .\n    * provide better data-types: packed unicode string by default, arrays.\n    .\n    * Better numerical classes that better represent mathematical thing (No more all-in-one Num).\n    .\n    * Better I/O system with less Lazy IO\n    .\n    * Usual partial functions distinguished through type system\nlicense:             BSD3\nlicense-file:        LICENSE\ncopyright:           2015-2017 Vincent Hanquez <vincent@snarc.org>, 2017- Foundation Maintainers\nauthor:              Vincent Hanquez <vincent@snarc.org>\nmaintainer:          vincent@snarc.org\ncategory:            foundation\nbuild-type:          Simple\nhomepage:            https://github.com/haskell-foundation/foundation\nbug-reports:         https://github.com/haskell-foundation/foundation/issues\ncabal-version:       1.18\nextra-source-files:  cbits/*.h\n\nsource-repository head\n  type: git\n  location: https://github.com/haskell-foundation/foundation\n  subdir: foundation\n\nflag experimental\n  description:       Enable building experimental features, known as highly unstable or without good support cross-platform\n  default:           False\n  manual:            True\n\nflag bench-all\n  description:       Add some comparaison benchmarks against other haskell libraries\n  default:           False\n  manual:            True\n\nflag minimal-deps\n  description:       Build fully with minimal deps (no criterion, no quickcheck, no doctest)\n  default:           False\n  manual:            True\n\nflag bounds-check\n  description:       Add extra friendly boundary check for unsafe array operations\n  default:           False\n  manual:            True\n\nflag doctest\n  description:       Build doctest on demand only\n  default:           False\n  manual:            True\n\nflag linktest\n  description:       Run linking test\n  default:           False\n  manual:            True\n\nlibrary\n  exposed-modules:   Foundation\n                     Foundation.Numerical\n                     Foundation.Array\n                     Foundation.Array.Internal\n                     Foundation.Bits\n                     Foundation.Class.Bifunctor\n                     Foundation.Class.Storable\n                     Foundation.Conduit\n                     Foundation.Conduit.Textual\n                     Foundation.Exception\n                     Foundation.Format.CSV\n                     Foundation.String\n                     Foundation.String.Read\n                     Foundation.String.Builder\n                     Foundation.IO\n                     Foundation.IO.FileMap\n                     Foundation.IO.Terminal\n                     Foundation.VFS\n                     Foundation.VFS.Path\n                     Foundation.VFS.FilePath\n                     Foundation.VFS.URI\n                     Foundation.Math.Trigonometry\n                     Foundation.Hashing\n                     Foundation.Foreign\n                     Foundation.Collection\n                     Foundation.Primitive\n                     Foundation.List.DList\n                     Foundation.List.ListN\n                     Foundation.Monad\n                     Foundation.Monad.Except\n                     Foundation.Monad.Reader\n                     Foundation.Monad.State\n                     Foundation.Network.IPv4\n                     Foundation.Network.IPv6\n                     Foundation.System.Info\n                     Foundation.Strict\n                     Foundation.Parser\n                     Foundation.Random\n                     Foundation.Check\n                     Foundation.Check.Main\n                     Foundation.Timing\n                     Foundation.Timing.Main\n                     Foundation.Time.Types\n                     Foundation.Time.Bindings\n                     Foundation.Time.StopWatch\n                     Foundation.Tuple.Nth\n                     Foundation.UUID\n                     Foundation.System.Entropy\n                     Foundation.System.Bindings\n  other-modules:\n                     Foundation.Tuple\n                     Foundation.Hashing.FNV\n                     Foundation.Hashing.SipHash\n                     Foundation.Hashing.Hasher\n                     Foundation.Hashing.Hashable\n                     Foundation.Check.Gen\n                     Foundation.Check.Print\n                     Foundation.Check.Arbitrary\n                     Foundation.Check.Property\n                     Foundation.Check.Config\n                     Foundation.Check.Types\n                     Foundation.Collection.Buildable\n                     Foundation.Collection.List\n                     Foundation.Collection.Element\n                     Foundation.Collection.InnerFunctor\n                     Foundation.Collection.Collection\n                     Foundation.Collection.Copy\n                     Foundation.Collection.Sequential\n                     Foundation.Collection.Keyed\n                     Foundation.Collection.Indexed\n                     Foundation.Collection.Foldable\n                     Foundation.Collection.Mutable\n                     Foundation.Collection.Zippable\n                     Foundation.Collection.Mappable\n                     Foundation.Conduit.Internal\n                     Foundation.Format.CSV.Types\n                     Foundation.Format.CSV.Builder\n                     Foundation.Format.CSV.Parser\n                     Foundation.Numerical.Floating\n                     Foundation.IO.File\n                     Foundation.Monad.MonadIO\n                     Foundation.Monad.Exception\n                     Foundation.Monad.Transformer\n                     Foundation.Monad.Identity\n                     Foundation.Monad.Base\n                     Foundation.Random.Class\n                     Foundation.Random.DRG\n                     Foundation.Random.ChaChaDRG\n                     Foundation.Random.XorShift\n                     Foundation.Array.Chunked.Unboxed\n                     Foundation.Array.Bitmap\n                     Foundation.Foreign.Alloc\n                     Foundation.Foreign.MemoryMap\n                     Foundation.Foreign.MemoryMap.Types\n                     Foundation.Partial\n                     -- Foundation.Time.Bindings\n                     Foundation.System.Entropy.Common\n                     Foundation.System.Bindings.Network\n                     Foundation.System.Bindings.Time\n                     Foundation.System.Bindings.Hs\n\n  include-dirs:      cbits\n  c-sources:         cbits/foundation_random.c\n                     cbits/foundation_network.c\n                     cbits/foundation_time.c\n                     cbits/foundation_utf8.c\n\n  if flag(experimental)\n    exposed-modules: Foundation.Network.HostName\n  if os(windows)\n    exposed-modules: Foundation.System.Bindings.Windows\n    other-modules:   Foundation.Foreign.MemoryMap.Windows\n                     Foundation.System.Entropy.Windows\n  else\n    exposed-modules: Foundation.System.Bindings.Posix\n                     Foundation.System.Bindings.PosixDef\n    other-modules:   Foundation.Foreign.MemoryMap.Posix\n                     Foundation.System.Entropy.Unix\n  if os(linux)\n    exposed-modules: Foundation.System.Bindings.Linux\n  if os(osx)\n    exposed-modules: Foundation.System.Bindings.Macos\n\n  default-extensions: NoImplicitPrelude\n                      RebindableSyntax\n                      TypeFamilies\n                      BangPatterns\n                      DeriveDataTypeable\n\n  if impl(ghc < 8.10) || impl(ghcjs)\n    buildable: False\n  else\n    build-depends:   base\n                   , ghc-prim\n    if os(windows)\n      build-depends: Win32\n      if arch(i386)\n        extra-libraries: gcc\n\n  build-depends: basement == 0.0.15\n\n  -- FIXME add suport for armel mipsel\n  --  CPP-options: -DARCH_IS_LITTLE_ENDIAN\n  -- FIXME add support for powerpc powerpc64 armeb mipseb\n  --  CPP-options: -DARCH_IS_BIG_ENDIAN\n  if (arch(i386) || arch(x86_64))\n    cpp-options: -DARCH_IS_LITTLE_ENDIAN\n  else\n    cpp-options: -DARCH_IS_UNKNOWN_ENDIAN\n  ghc-options:       -Wall -fwarn-tabs -Wno-redundant-constraints\n  default-language:  Haskell2010\n  if flag(bounds-check)\n    cpp-options: -DFOUNDATION_BOUNDS_CHECK\n\ntest-suite check-foundation\n  type:              exitcode-stdio-1.0\n  hs-source-dirs:    tests\n  main-is:           Checks.hs\n  other-modules:     Test.Checks.Property.Collection\n                     Test.Foundation.Random\n                     Test.Foundation.Misc\n                     Test.Foundation.Conduit\n                     Test.Foundation.Primitive.BlockN\n                     Test.Foundation.Storable\n                     Test.Foundation.Number\n                     Test.Foundation.String.Base64\n                     Test.Foundation.String\n                     Test.Foundation.Bits\n                     Test.Basement\n                     Test.Basement.UTF8\n                     Test.Data.Network\n                     Test.Data.List\n                     Test.Foundation.Network.IPv4\n                     Test.Foundation.Network.IPv6\n                     Test.Foundation.Format\n                     Test.Foundation.Format.CSV\n  default-extensions: NoImplicitPrelude\n                      RebindableSyntax\n                      OverloadedStrings\n  build-depends:     base > 0 && < 1000\n                   , basement\n                   , foundation\n  ghc-options:       -Wall -fno-warn-orphans -fno-warn-missing-signatures -Wno-redundant-constraints\n  default-language:  Haskell2010\n\ntest-suite foundation-link\n  type:              exitcode-stdio-1.0\n  hs-source-dirs:    tests\n  default-language:  Haskell2010\n  main-is:           Scripts/Link.hs\n  default-extensions: NoImplicitPrelude\n                      RebindableSyntax\n  if flag(linktest)\n    build-depends:     base > 0 && < 1000\n                     , foundation\n                     , template-haskell\n    buildable:     True\n  else\n    buildable:     False\n\ntest-suite doctest\n  type:              exitcode-stdio-1.0\n  hs-source-dirs:    tests\n  default-language:  Haskell2010\n  main-is:           DocTest.hs\n  default-extensions: NoImplicitPrelude\n                      RebindableSyntax\n  if flag(minimal-deps)\n    -- TODO: for no, force unbuildable anyway\n    buildable:       False\n  else\n    if flag(doctest)\n      build-depends:     base\n                       , doctest >= 0.9\n      buildable:     True\n    else\n      buildable:     False\n\nBenchmark bench\n  main-is:           Main.hs\n  other-modules:     BenchUtil.Common\n                     BenchUtil.RefData\n                     Sys\n                     LargeWords\n                     Fake.ByteString\n                     Fake.Text\n                     Fake.Vector\n  hs-source-dirs:    benchs\n  default-language:  Haskell2010\n  type:              exitcode-stdio-1.0\n  default-extensions: NoImplicitPrelude\n                      BangPatterns\n  if flag(minimal-deps) || impl(ghc < 7.10)\n    buildable: False\n  else\n    build-depends:     base, gauge, basement, foundation\n    if flag(bench-all)\n      cpp-options:     -DBENCH_ALL\n      build-depends:   text, attoparsec, vector, bytestring\n";
    }