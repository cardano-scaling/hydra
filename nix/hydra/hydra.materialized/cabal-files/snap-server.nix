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
      portable = false;
      openssl = false;
      build-pong = false;
      build-testserver = false;
      debug = false;
      };
    package = {
      specVersion = "1.10";
      identifier = { name = "snap-server"; version = "1.1.2.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "snap@snapframework.com";
      author = "Snap Framework Authors  (see CONTRIBUTORS)";
      homepage = "http://snapframework.com/";
      url = "";
      synopsis = "A web server for the Snap Framework";
      description = "Snap is a simple and fast web development framework and server written in\nHaskell. For more information or to download the latest version, you can\nvisit the Snap project website at <http://snapframework.com/>.\n\nThe Snap HTTP server is a high performance web server library written in\nHaskell. Together with the @snap-core@ library upon which it depends, it\nprovides a clean and efficient Haskell programming interface to the HTTP\nprotocol.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = (([
          (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."blaze-builder" or (errorHandler.buildDepError "blaze-builder"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."bytestring-builder" or (errorHandler.buildDepError "bytestring-builder"))
          (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
          (hsPkgs."clock" or (errorHandler.buildDepError "clock"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."io-streams" or (errorHandler.buildDepError "io-streams"))
          (hsPkgs."io-streams-haproxy" or (errorHandler.buildDepError "io-streams-haproxy"))
          (hsPkgs."lifted-base" or (errorHandler.buildDepError "lifted-base"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."old-locale" or (errorHandler.buildDepError "old-locale"))
          (hsPkgs."snap-core" or (errorHandler.buildDepError "snap-core"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."unix-compat" or (errorHandler.buildDepError "unix-compat"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))) ++ (pkgs.lib).optional (!(flags.portable || system.isWindows)) (hsPkgs."unix" or (errorHandler.buildDepError "unix"))) ++ (pkgs.lib).optionals (flags.openssl) [
          (hsPkgs."HsOpenSSL" or (errorHandler.buildDepError "HsOpenSSL"))
          (hsPkgs."openssl-streams" or (errorHandler.buildDepError "openssl-streams"))
          ];
        buildable = true;
        };
      exes = {
        "snap-test-pong-server" = {
          depends = ([
            (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."blaze-builder" or (errorHandler.buildDepError "blaze-builder"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."bytestring-builder" or (errorHandler.buildDepError "bytestring-builder"))
            (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
            (hsPkgs."clock" or (errorHandler.buildDepError "clock"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."io-streams" or (errorHandler.buildDepError "io-streams"))
            (hsPkgs."io-streams-haproxy" or (errorHandler.buildDepError "io-streams-haproxy"))
            (hsPkgs."lifted-base" or (errorHandler.buildDepError "lifted-base"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."old-locale" or (errorHandler.buildDepError "old-locale"))
            (hsPkgs."snap-core" or (errorHandler.buildDepError "snap-core"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."unix-compat" or (errorHandler.buildDepError "unix-compat"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ] ++ (pkgs.lib).optional (!(flags.portable || system.isWindows)) (hsPkgs."unix" or (errorHandler.buildDepError "unix"))) ++ (pkgs.lib).optionals (flags.openssl) [
            (hsPkgs."HsOpenSSL" or (errorHandler.buildDepError "HsOpenSSL"))
            (hsPkgs."openssl-streams" or (errorHandler.buildDepError "openssl-streams"))
            ];
          buildable = if !flags.build-pong then false else true;
          };
        "snap-test-server" = {
          depends = ([
            (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."blaze-builder" or (errorHandler.buildDepError "blaze-builder"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."bytestring-builder" or (errorHandler.buildDepError "bytestring-builder"))
            (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
            (hsPkgs."clock" or (errorHandler.buildDepError "clock"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."io-streams" or (errorHandler.buildDepError "io-streams"))
            (hsPkgs."io-streams-haproxy" or (errorHandler.buildDepError "io-streams-haproxy"))
            (hsPkgs."lifted-base" or (errorHandler.buildDepError "lifted-base"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."old-locale" or (errorHandler.buildDepError "old-locale"))
            (hsPkgs."snap-core" or (errorHandler.buildDepError "snap-core"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."unix-compat" or (errorHandler.buildDepError "unix-compat"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ] ++ (pkgs.lib).optionals (flags.openssl) [
            (hsPkgs."HsOpenSSL" or (errorHandler.buildDepError "HsOpenSSL"))
            (hsPkgs."openssl-streams" or (errorHandler.buildDepError "openssl-streams"))
            ]) ++ (pkgs.lib).optional (!(flags.portable || system.isWindows)) (hsPkgs."unix" or (errorHandler.buildDepError "unix"));
          buildable = if !flags.build-testserver then false else true;
          };
        };
      tests = {
        "testsuite" = {
          depends = (([
            (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."blaze-builder" or (errorHandler.buildDepError "blaze-builder"))
            (hsPkgs."bytestring-builder" or (errorHandler.buildDepError "bytestring-builder"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
            (hsPkgs."clock" or (errorHandler.buildDepError "clock"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."io-streams" or (errorHandler.buildDepError "io-streams"))
            (hsPkgs."io-streams-haproxy" or (errorHandler.buildDepError "io-streams-haproxy"))
            (hsPkgs."lifted-base" or (errorHandler.buildDepError "lifted-base"))
            (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."old-locale" or (errorHandler.buildDepError "old-locale"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."snap-core" or (errorHandler.buildDepError "snap-core"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."threads" or (errorHandler.buildDepError "threads"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."unix-compat" or (errorHandler.buildDepError "unix-compat"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."http-streams" or (errorHandler.buildDepError "http-streams"))
            (hsPkgs."http-common" or (errorHandler.buildDepError "http-common"))
            (hsPkgs."parallel" or (errorHandler.buildDepError "parallel"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
            ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))) ++ (pkgs.lib).optional (!(flags.portable || system.isWindows)) (hsPkgs."unix" or (errorHandler.buildDepError "unix"))) ++ (pkgs.lib).optionals (flags.openssl) [
            (hsPkgs."HsOpenSSL" or (errorHandler.buildDepError "HsOpenSSL"))
            (hsPkgs."openssl-streams" or (errorHandler.buildDepError "openssl-streams"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "benchmark" = {
          depends = [
            (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."blaze-builder" or (errorHandler.buildDepError "blaze-builder"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."bytestring-builder" or (errorHandler.buildDepError "bytestring-builder"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."io-streams" or (errorHandler.buildDepError "io-streams"))
            (hsPkgs."io-streams-haproxy" or (errorHandler.buildDepError "io-streams-haproxy"))
            (hsPkgs."snap-core" or (errorHandler.buildDepError "snap-core"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/snap-server-1.1.2.0.tar.gz";
      sha256 = "92306f4148fd9eca06a608b9a8d46a95e928aee231ab320650f5d25854da9e70";
      });
    }) // {
    package-description-override = "name:           snap-server\r\nversion:        1.1.2.0\r\nx-revision: 1\r\nsynopsis:       A web server for the Snap Framework\r\ndescription:\r\n  Snap is a simple and fast web development framework and server written in\r\n  Haskell. For more information or to download the latest version, you can\r\n  visit the Snap project website at <http://snapframework.com/>.\r\n  .\r\n  The Snap HTTP server is a high performance web server library written in\r\n  Haskell. Together with the @snap-core@ library upon which it depends, it\r\n  provides a clean and efficient Haskell programming interface to the HTTP\r\n  protocol.\r\n\r\nlicense:        BSD3\r\nlicense-file:   LICENSE\r\nauthor:         Snap Framework Authors  (see CONTRIBUTORS)\r\nmaintainer:     snap@snapframework.com\r\nbuild-type:     Simple\r\ncabal-version:  >= 1.10\r\nhomepage:       http://snapframework.com/\r\nbug-reports:    https://github.com/snapframework/snap-server/issues\r\ncategory:       Web, Snap, IO-Streams\r\n\r\nextra-source-files:\r\n  CONTRIBUTORS,\r\n  LICENSE,\r\n  README.md,\r\n  README.SNAP.md,\r\n  test/bad_key.pem,\r\n  test/cert.pem,\r\n  test/dummy.txt,\r\n  test/key.pem,\r\n  testserver/static/hello.txt\r\n\r\ntested-with:\r\n  GHC==7.6.3,\r\n  GHC==7.8.4,\r\n  GHC==7.10.3,\r\n  GHC==8.0.2,\r\n  GHC==8.2.2,\r\n  GHC==8.4.4,\r\n  GHC==8.6.5,\r\n  GHC==8.8.3,\r\n  GHC==8.10.1\r\n\r\nFlag portable\r\n  Description: Compile in cross-platform mode. No platform-specific code or\r\n               optimizations such as C routines will be used.\r\n  Default: False\r\n\r\nFlag openssl\r\n  Description: Enable https support using the HsOpenSSL library.\r\n  Default: False\r\n  Manual: True\r\n\r\nFlag build-pong\r\n  Description: Build a server that just returns \"PONG\"? Normally useful only\r\n               for benchmarks.\r\n  Default: False\r\n  Manual: True\r\n\r\nFlag build-testserver\r\n  Description: Build the blackbox testserver?\r\n  Default: False\r\n  Manual: True\r\n\r\nFlag debug\r\n  Description: Enable support for debugging.\r\n  Default: False\r\n  Manual: True\r\n\r\nLibrary\r\n  hs-source-dirs:    src\r\n  Default-language:  Haskell2010\r\n\r\n  exposed-modules:\r\n    Snap.Http.Server,\r\n    Snap.Http.Server.Config,\r\n    Snap.Http.Server.Types,\r\n    Snap.Internal.Http.Server.Config,\r\n    Snap.Internal.Http.Server.Types,\r\n    System.FastLogger\r\n\r\n  other-modules:\r\n    Paths_snap_server,\r\n    Control.Concurrent.Extended,\r\n    Snap.Internal.Http.Server.Address,\r\n    Snap.Internal.Http.Server.Clock,\r\n    Snap.Internal.Http.Server.Common,\r\n    Snap.Internal.Http.Server.Date,\r\n    Snap.Internal.Http.Server.Parser,\r\n    Snap.Internal.Http.Server.Session,\r\n    Snap.Internal.Http.Server.Socket,\r\n    Snap.Internal.Http.Server.Thread,\r\n    Snap.Internal.Http.Server.TimeoutManager,\r\n    Snap.Internal.Http.Server.TLS\r\n\r\n  build-depends:\r\n    attoparsec                          >= 0.12     && < 0.14,\r\n    base                                >= 4.6      && < 4.16,\r\n    blaze-builder                       >= 0.4      && < 0.5,\r\n    bytestring                          >= 0.9.1    && < 0.12,\r\n    bytestring-builder                  >= 0.10.4   && < 0.11,\r\n    case-insensitive                    >= 1.1      && < 1.3,\r\n    clock                               >= 0.7.1    && < 0.9,\r\n    containers                          >= 0.3      && < 0.7,\r\n    filepath                            >= 1.1      && < 2.0,\r\n    io-streams                          >= 1.3      && < 1.6,\r\n    io-streams-haproxy                  >= 1.0      && < 1.1,\r\n    lifted-base                         >= 0.1      && < 0.3,\r\n    mtl                                 >= 2.0      && < 2.3,\r\n    network                             >= 2.3      && < 3.2,\r\n    old-locale                          >= 1.0      && < 1.1,\r\n    snap-core                           >= 1.0      && < 1.1,\r\n    text                                >= 0.11     && < 1.3,\r\n    time                                >= 1.0      && < 1.13,\r\n    transformers                        >= 0.3      && < 0.6,\r\n    unix-compat                         >= 0.2      && < 0.6,\r\n    vector                              >= 0.7      && < 0.13\r\n\r\n  other-extensions:\r\n    BangPatterns,\r\n    CPP,\r\n    MagicHash,\r\n    Rank2Types,\r\n    OverloadedStrings,\r\n    ScopedTypeVariables,\r\n    DeriveDataTypeable,\r\n    PackageImports,\r\n    ViewPatterns,\r\n    ForeignFunctionInterface,\r\n    EmptyDataDecls,\r\n    GeneralizedNewtypeDeriving\r\n\r\n  if !impl(ghc >= 8.0)\r\n    build-depends: semigroups >= 0.16 && < 0.19\r\n\r\n  if flag(portable) || os(windows)\r\n    cpp-options: -DPORTABLE\r\n  else\r\n    build-depends: unix                         < 2.8\r\n\r\n  if flag(openssl)\r\n    cpp-options: -DOPENSSL\r\n    build-depends: HsOpenSSL       >= 0.10.4 && < 0.12,\r\n                   openssl-streams >= 1.1    && < 1.3\r\n\r\n  if os(linux) && !flag(portable)\r\n    cpp-options: -DLINUX -DHAS_SENDFILE -DHAS_UNIX_SOCKETS\r\n    other-modules:\r\n      System.SendFile,\r\n      System.SendFile.Linux\r\n\r\n-- Disabling sendfile() on OSX for now. See\r\n--\r\n-- https://github.com/snapframework/snap-core/issues/274 and\r\n-- https://github.com/snapframework/snap-core/issues/91\r\n--\r\n  if os(darwin) && !flag(portable)\r\n     cpp-options: -DHAS_UNIX_SOCKETS\r\n  -- if os(darwin) && !flag(portable)\r\n  --   cpp-options: -DOSX -DHAS_UNIX_SOCKETS\r\n\r\n  if os(freebsd) && !flag(portable)\r\n    cpp-options: -DFREEBSD -DHAS_SENDFILE -DHAS_UNIX_SOCKETS\r\n    other-modules:\r\n      System.SendFile,\r\n      System.SendFile.FreeBSD\r\n\r\n  if impl(ghc >= 6.12.0)\r\n    ghc-options: -Wall -fwarn-tabs -funbox-strict-fields -fno-warn-unused-do-bind\r\n  else\r\n    ghc-options: -Wall -fwarn-tabs -funbox-strict-fields\r\n\r\n  if flag(debug)\r\n    cpp-options: -DLABEL_THREADS\r\n\r\nTest-suite testsuite\r\n  hs-source-dirs:    src test\r\n  Type:              exitcode-stdio-1.0\r\n  Main-is:           TestSuite.hs\r\n  Default-language:  Haskell2010\r\n\r\n  other-modules:\r\n    Control.Concurrent.Extended,\r\n    Paths_snap_server,\r\n    Snap.Http.Server,\r\n    Snap.Http.Server.Config,\r\n    Snap.Http.Server.Types,\r\n    Snap.Internal.Http.Server.Address,\r\n    Snap.Internal.Http.Server.Clock,\r\n    Snap.Internal.Http.Server.Common,\r\n    Snap.Internal.Http.Server.Config,\r\n    Snap.Internal.Http.Server.Date,\r\n    Snap.Internal.Http.Server.Parser,\r\n    Snap.Internal.Http.Server.Session,\r\n    Snap.Internal.Http.Server.Socket,\r\n    Snap.Internal.Http.Server.Thread,\r\n    Snap.Internal.Http.Server.TimeoutManager,\r\n    Snap.Internal.Http.Server.TLS\r\n    Snap.Internal.Http.Server.Types,\r\n    System.FastLogger,\r\n\r\n    Snap.Internal.Http.Server.Address.Tests,\r\n    Snap.Internal.Http.Server.Parser.Tests,\r\n    Snap.Internal.Http.Server.Session.Tests,\r\n    Snap.Internal.Http.Server.Socket.Tests,\r\n    Snap.Internal.Http.Server.TimeoutManager.Tests,\r\n    Snap.Test.Common,\r\n    Test.Blackbox,\r\n    Test.Common.Rot13,\r\n    Test.Common.TestHandler\r\n\r\n  build-depends:\r\n    attoparsec,\r\n    base,\r\n    base16-bytestring                   >= 0.1      && < 1.1,\r\n    blaze-builder,\r\n    bytestring-builder,\r\n    bytestring,\r\n    case-insensitive,\r\n    clock,\r\n    containers,\r\n    directory                           >= 1.1      && < 1.4,\r\n    filepath,\r\n    io-streams,\r\n    io-streams-haproxy,\r\n    lifted-base,\r\n    monad-control                       >= 1.0      && < 1.1,\r\n    mtl,\r\n    network,\r\n    old-locale,\r\n    random                              >= 1.0      && < 1.3,\r\n    snap-core,\r\n    text,\r\n    threads                             >= 0.5      && < 0.6,\r\n    time,\r\n    transformers                        >= 0.3      && < 0.6,\r\n    unix-compat,\r\n    vector,\r\n\r\n    HUnit                               >= 1.2      && < 2,\r\n    QuickCheck                          >= 2.3.0.2  && < 3,\r\n    deepseq                             >= 1.3      && < 2,\r\n    http-streams                        >= 0.7      && < 0.9,\r\n    http-common                         >= 0.7      && < 0.9,\r\n    parallel                            >= 3        && < 4,\r\n    test-framework                      >= 0.8.0.3  && < 0.9,\r\n    test-framework-hunit                >= 0.2.7    && < 0.4,\r\n    test-framework-quickcheck2          >= 0.2.12.1 && < 0.4\r\n\r\n  other-extensions:\r\n    BangPatterns,\r\n    CPP,\r\n    MagicHash,\r\n    Rank2Types,\r\n    OverloadedStrings,\r\n    ScopedTypeVariables,\r\n    DeriveDataTypeable,\r\n    PackageImports,\r\n    ViewPatterns,\r\n    ForeignFunctionInterface,\r\n    EmptyDataDecls,\r\n    GeneralizedNewtypeDeriving\r\n\r\n  if !impl(ghc >= 8.0)\r\n    build-depends: semigroups\r\n\r\n  if flag(portable) || os(windows)\r\n    cpp-options: -DPORTABLE\r\n  else\r\n    build-depends: unix\r\n\r\n  -- always label threads in testsuite\r\n  cpp-options: -DLABEL_THREADS\r\n\r\n  if flag(openssl)\r\n    cpp-options: -DOPENSSL\r\n    build-depends: HsOpenSSL,\r\n                   openssl-streams\r\n\r\n  if os(linux) && !flag(portable)\r\n    cpp-options: -DLINUX -DHAS_SENDFILE -DHAS_UNIX_SOCKETS\r\n    other-modules:\r\n      System.SendFile,\r\n      System.SendFile.Linux,\r\n      System.SendFile.Tests\r\n    c-sources: test/cbits/errno_util.c\r\n\r\n  if os(darwin) && !flag(portable)\r\n     cpp-options: -DHAS_UNIX_SOCKETS\r\n--  if os(darwin) && !flag(portable)\r\n--    cpp-options: -DOSX -DHAS_SENDFILE -DHAS_UNIX_SOCKETS\r\n--    other-modules:\r\n--      System.SendFile,\r\n--      System.SendFile.Darwin,\r\n--      System.SendFile.Tests\r\n--    c-sources: test/cbits/errno_util.c\r\n\r\n  if os(freebsd) && !flag(portable)\r\n    cpp-options: -DFREEBSD -DHAS_SENDFILE -DHAS_UNIX_SOCKETS\r\n    other-modules:\r\n      System.SendFile,\r\n      System.SendFile.FreeBSD,\r\n      System.SendFile.Tests\r\n    c-sources: test/cbits/errno_util.c\r\n\r\n  cpp-options: -DTESTSUITE\r\n\r\n  ghc-options: -Wall -fwarn-tabs -funbox-strict-fields\r\n               -fno-warn-unused-do-bind -threaded\r\n\r\n\r\nBenchmark benchmark\r\n  type:             exitcode-stdio-1.0\r\n  hs-source-dirs:   benchmark src\r\n  main-is:          Benchmark.hs\r\n  default-language: Haskell2010\r\n\r\n  other-modules:\r\n    Snap.Internal.Http.Parser.Benchmark,\r\n    Snap.Internal.Http.Parser.Data,\r\n    Snap.Internal.Http.Server.Parser\r\n\r\n  build-depends:\r\n    attoparsec,\r\n    base,\r\n    blaze-builder,\r\n    bytestring,\r\n    bytestring-builder,\r\n    criterion                           >= 0.6     && < 1.6,\r\n    io-streams,\r\n    io-streams-haproxy,\r\n    snap-core,\r\n    transformers,\r\n    vector\r\n\r\n  ghc-options: -Wall -fwarn-tabs -funbox-strict-fields\r\n               -fno-warn-unused-do-bind -rtsopts\r\n\r\n  other-extensions:\r\n    BangPatterns,\r\n    CPP,\r\n    MagicHash,\r\n    Rank2Types,\r\n    OverloadedStrings,\r\n    ScopedTypeVariables,\r\n    DeriveDataTypeable,\r\n    PackageImports,\r\n    ViewPatterns,\r\n    ForeignFunctionInterface,\r\n    EmptyDataDecls,\r\n    GeneralizedNewtypeDeriving\r\n\r\nExecutable snap-test-pong-server\r\n  hs-source-dirs: src pong\r\n  main-is: Main.hs\r\n\r\n  if !flag(build-pong)\r\n    buildable: False\r\n\r\n  default-language: Haskell2010\r\n\r\n  other-modules:\r\n    Paths_snap_server,\r\n    Snap.Internal.Http.Server.Address,\r\n    Snap.Internal.Http.Server.Clock,\r\n    Snap.Internal.Http.Server.Common,\r\n    Snap.Internal.Http.Server.Config,\r\n    Snap.Internal.Http.Server.Date,\r\n    Snap.Internal.Http.Server.Parser,\r\n    Snap.Internal.Http.Server.Session,\r\n    Snap.Internal.Http.Server.Socket,\r\n    Snap.Internal.Http.Server.Thread,\r\n    Snap.Internal.Http.Server.TimeoutManager,\r\n    Snap.Internal.Http.Server.TLS,\r\n    Snap.Internal.Http.Server.Types\r\n\r\n  if flag(portable) || os(windows)\r\n    cpp-options: -DPORTABLE\r\n  else\r\n    build-depends: unix\r\n\r\n  if os(linux) && !flag(portable)\r\n    cpp-options: -DLINUX -DHAS_SENDFILE -DHAS_UNIX_SOCKETS\r\n    other-modules:\r\n      System.SendFile,\r\n      System.SendFile.Linux\r\n\r\n  if os(darwin) && !flag(portable)\r\n     cpp-options: -DHAS_UNIX_SOCKETS\r\n--  if os(darwin) && !flag(portable)\r\n--    cpp-options: -DOSX -DHAS_SENDFILE -DHAS_UNIX_SOCKETS\r\n--    other-modules:\r\n--      System.SendFile,\r\n--      System.SendFile.Darwin\r\n\r\n  if os(freebsd) && !flag(portable)\r\n    cpp-options: -DFREEBSD -DHAS_SENDFILE -DHAS_UNIX_SOCKETS\r\n    other-modules:\r\n      System.SendFile,\r\n      System.SendFile.FreeBSD\r\n\r\n  if flag(openssl)\r\n    cpp-options: -DOPENSSL\r\n    build-depends: HsOpenSSL,\r\n                   openssl-streams\r\n\r\n  build-depends:\r\n    attoparsec,\r\n    base,\r\n    blaze-builder,\r\n    bytestring,\r\n    bytestring-builder,\r\n    case-insensitive,\r\n    clock,\r\n    containers,\r\n    io-streams,\r\n    io-streams-haproxy,\r\n    lifted-base,\r\n    mtl,\r\n    network,\r\n    old-locale,\r\n    snap-core,\r\n    text,\r\n    time,\r\n    unix-compat,\r\n    vector\r\n\r\n  ghc-options: -Wall -fwarn-tabs -funbox-strict-fields\r\n               -fno-warn-unused-do-bind -threaded -rtsopts\r\n\r\n  other-extensions:\r\n    BangPatterns,\r\n    CPP,\r\n    MagicHash,\r\n    Rank2Types,\r\n    OverloadedStrings,\r\n    ScopedTypeVariables,\r\n    DeriveDataTypeable,\r\n    PackageImports,\r\n    ViewPatterns,\r\n    ForeignFunctionInterface,\r\n    EmptyDataDecls,\r\n    GeneralizedNewtypeDeriving\r\n\r\n\r\nExecutable snap-test-server\r\n  hs-source-dirs: src testserver test\r\n  main-is: Main.hs\r\n\r\n  if !flag(build-testserver)\r\n    buildable: False\r\n\r\n  if flag(openssl)\r\n    cpp-options: -DOPENSSL\r\n    build-depends: HsOpenSSL,\r\n                   openssl-streams\r\n\r\n  default-language: Haskell2010\r\n\r\n  other-modules:\r\n    Paths_snap_server,\r\n    Snap.Internal.Http.Server.Address,\r\n    Snap.Internal.Http.Server.Clock,\r\n    Snap.Internal.Http.Server.Common,\r\n    Snap.Internal.Http.Server.Config,\r\n    Snap.Internal.Http.Server.Date,\r\n    Snap.Internal.Http.Server.Parser,\r\n    Snap.Internal.Http.Server.Session,\r\n    Snap.Internal.Http.Server.Socket,\r\n    Snap.Internal.Http.Server.Thread,\r\n    Snap.Internal.Http.Server.TimeoutManager,\r\n    Snap.Internal.Http.Server.TLS,\r\n    Snap.Internal.Http.Server.Types\r\n\r\n  if flag(portable) || os(windows)\r\n    cpp-options: -DPORTABLE\r\n  else\r\n    build-depends: unix\r\n\r\n  if os(linux) && !flag(portable)\r\n    cpp-options: -DLINUX -DHAS_SENDFILE -DHAS_UNIX_SOCKETS\r\n    other-modules:\r\n      System.SendFile,\r\n      System.SendFile.Linux\r\n\r\n  if os(darwin) && !flag(portable)\r\n     cpp-options: -DHAS_UNIX_SOCKETS\r\n  -- if os(darwin) && !flag(portable)\r\n  --   cpp-options: -DOSX -DHAS_SENDFILE -DHAS_UNIX_SOCKETS\r\n  --   other-modules:\r\n  --     System.SendFile,\r\n  --     System.SendFile.Darwin\r\n\r\n  if os(freebsd) && !flag(portable)\r\n    cpp-options: -DFREEBSD -DHAS_SENDFILE -DHAS_UNIX_SOCKETS\r\n    other-modules:\r\n      System.SendFile,\r\n      System.SendFile.FreeBSD\r\n\r\n  build-depends:\r\n    attoparsec,\r\n    base,\r\n    blaze-builder,\r\n    bytestring,\r\n    bytestring-builder,\r\n    case-insensitive,\r\n    clock,\r\n    containers,\r\n    directory,\r\n    io-streams,\r\n    io-streams-haproxy,\r\n    lifted-base,\r\n    mtl,\r\n    network,\r\n    old-locale,\r\n    snap-core,\r\n    text,\r\n    time,\r\n    transformers,\r\n    unix-compat,\r\n    vector\r\n\r\n  ghc-options: -Wall -fwarn-tabs -funbox-strict-fields\r\n               -fno-warn-unused-do-bind -threaded -rtsopts\r\n\r\n  other-extensions:\r\n    BangPatterns,\r\n    CPP,\r\n    MagicHash,\r\n    Rank2Types,\r\n    OverloadedStrings,\r\n    ScopedTypeVariables,\r\n    DeriveDataTypeable,\r\n    PackageImports,\r\n    ViewPatterns,\r\n    ForeignFunctionInterface,\r\n    EmptyDataDecls,\r\n    GeneralizedNewtypeDeriving\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: git://github.com/snapframework/snap-server.git\r\n";
    }