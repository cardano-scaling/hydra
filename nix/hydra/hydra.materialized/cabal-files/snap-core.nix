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
    flags = { portable = false; debug = false; network-uri = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "snap-core"; version = "1.0.5.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "snap@snapframework.com";
      author = "Snap Framework Authors (see CONTRIBUTORS)";
      homepage = "http://snapframework.com/";
      url = "";
      synopsis = "Snap: A Haskell Web Framework (core interfaces and types)";
      description = "Snap is a simple and fast web development framework and server written in\nHaskell. For more information or to download the latest version, you can\nvisit the Snap project website at <http://snapframework.com/>.\n\nThis library contains the core definitions and types for the Snap framework,\nincluding:\n\n1. Primitive types and functions for HTTP (requests, responses, cookies,\npost/query parameters, etc)\n\n2. A monad for programming web handlers called \\\"Snap\\\", which allows:\n\n* Stateful access to the HTTP request and response objects\n\n* Monadic failure (i.e. MonadPlus/Alternative instances) for declining\nto handle requests and chaining handlers together\n\n* Early termination of the computation if you know early what you want\nto return and want to prevent further monadic processing\n\n/Quick start/: The 'Snap' monad and HTTP definitions are in \"Snap.Core\".";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ((([
          (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
          (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."bytestring-builder" or (errorHandler.buildDepError "bytestring-builder"))
          (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."lifted-base" or (errorHandler.buildDepError "lifted-base"))
          (hsPkgs."io-streams" or (errorHandler.buildDepError "io-streams"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."readable" or (errorHandler.buildDepError "readable"))
          (hsPkgs."regex-posix" or (errorHandler.buildDepError "regex-posix"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
          (hsPkgs."unix-compat" or (errorHandler.buildDepError "unix-compat"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ] ++ (if flags.portable || system.isWindows
          then [
            (hsPkgs."time-locale-compat" or (errorHandler.buildDepError "time-locale-compat"))
            ]
          else [
            (hsPkgs."old-locale" or (errorHandler.buildDepError "old-locale"))
            ])) ++ (pkgs.lib).optionals (compiler.isGhc && (compiler.version).lt "8.8") ((pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) [
          (hsPkgs."fail" or (errorHandler.buildDepError "fail"))
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          ])) ++ [
          (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          ]) ++ [
          (hsPkgs."unix-compat" or (errorHandler.buildDepError "unix-compat"))
          ];
        buildable = true;
        };
      tests = {
        "testsuite" = {
          depends = (([
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."bytestring-builder" or (errorHandler.buildDepError "bytestring-builder"))
            (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."lifted-base" or (errorHandler.buildDepError "lifted-base"))
            (hsPkgs."io-streams" or (errorHandler.buildDepError "io-streams"))
            (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."readable" or (errorHandler.buildDepError "readable"))
            (hsPkgs."regex-posix" or (errorHandler.buildDepError "regex-posix"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
            (hsPkgs."unix-compat" or (errorHandler.buildDepError "unix-compat"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."parallel" or (errorHandler.buildDepError "parallel"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
            (hsPkgs."zlib" or (errorHandler.buildDepError "zlib"))
            ] ++ (if flags.portable || system.isWindows
            then [
              (hsPkgs."time-locale-compat" or (errorHandler.buildDepError "time-locale-compat"))
              ]
            else [
              (hsPkgs."old-locale" or (errorHandler.buildDepError "old-locale"))
              ])) ++ [
            (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            ]) ++ (pkgs.lib).optionals (compiler.isGhc && (compiler.version).lt "8.8") ((pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) [
            (hsPkgs."fail" or (errorHandler.buildDepError "fail"))
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            ]);
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/snap-core-1.0.5.0.tar.gz";
      sha256 = "642aedd244865560b5d656cd460e829a39d859ad031710b79bd112785e38c641";
      });
    }) // {
    package-description-override = "name:           snap-core\r\nversion:        1.0.5.0\r\nx-revision: 2\r\nsynopsis:       Snap: A Haskell Web Framework (core interfaces and types)\r\n\r\ndescription:\r\n  Snap is a simple and fast web development framework and server written in\r\n  Haskell. For more information or to download the latest version, you can\r\n  visit the Snap project website at <http://snapframework.com/>.\r\n  .\r\n  This library contains the core definitions and types for the Snap framework,\r\n  including:\r\n  .\r\n    1. Primitive types and functions for HTTP (requests, responses, cookies,\r\n       post/query parameters, etc)\r\n  .\r\n    2. A monad for programming web handlers called \\\"Snap\\\", which allows:\r\n  .\r\n       * Stateful access to the HTTP request and response objects\r\n  .\r\n       * Monadic failure (i.e. MonadPlus/Alternative instances) for declining\r\n         to handle requests and chaining handlers together\r\n  .\r\n       * Early termination of the computation if you know early what you want\r\n         to return and want to prevent further monadic processing\r\n  .\r\n  /Quick start/: The 'Snap' monad and HTTP definitions are in \"Snap.Core\".\r\n\r\nlicense:        BSD3\r\nlicense-file:   LICENSE\r\nauthor:         Snap Framework Authors (see CONTRIBUTORS)\r\nmaintainer:     snap@snapframework.com\r\nbuild-type:     Simple\r\ncabal-version:  >= 1.10\r\nhomepage:       http://snapframework.com/\r\nbug-reports:    https://github.com/snapframework/snap-core/issues\r\ncategory:       Web, Snap, IO-Streams\r\nTested-With:    GHC == 7.4.2, GHC == 7.6.3, GHC == 7.8.4, GHC == 7.10.3,\r\n                GHC == 8.0.2, GHC == 8.2.2, GHC == 8.4.4, GHC == 8.6.5,\r\n                GHC == 8.8.4, GHC == 8.10.7, GHC == 9.0.2, GHC == 9.2.1\r\n\r\nextra-source-files:\r\n  test/TestSuite.hs,\r\n  cbits/timefuncs.c,\r\n  CONTRIBUTORS,\r\n  extra/haddock.css,\r\n  extra/hscolour.css,\r\n  extra/logo.gif,\r\n  haddock.sh,\r\n  LICENSE,\r\n  README.md,\r\n  README.SNAP.md,\r\n  Setup.hs,\r\n  runTestsAndCoverage.sh,\r\n  test/data/fileServe/foo.bin,\r\n  test/data/fileServe/foo.bin.bin.bin,\r\n  test/data/fileServe/foo.html,\r\n  test/data/fileServe/foo.txt,\r\n  test/data/fileServe/mydir1/index.txt,\r\n  test/data/fileServe/mydir2/dir/foo.txt,\r\n  test/data/fileServe/mydir2/foo.txt,\r\n  test/data/fileServe/mydir3/altindex.html,\r\n  test/Snap/Core/Tests.hs,\r\n  test/Snap/Internal/Http/Types/Tests.hs,\r\n  test/Snap/Internal/Parsing/Tests.hs,\r\n  test/Snap/Internal/Routing/Tests.hs,\r\n  test/Snap/Test/Common.hs,\r\n  test/Snap/Types/Headers/Tests.hs,\r\n  test/Snap/Util/FileServe/Tests.hs,\r\n  test/Snap/Util/FileUploads/Tests.hs,\r\n  test/Snap/Util/GZip/Tests.hs,\r\n  test/Snap/Util/Proxy/Tests.hs\r\n\r\nFlag portable\r\n  Description: Compile in cross-platform mode. No platform-specific code or\r\n               optimizations such as C routines will be used.\r\n  Default: False\r\n\r\n\r\nFlag debug\r\n  Description: Enable debug logging code. With this flag, Snap will\r\n               test the DEBUG environment variable to decide whether to do\r\n               logging, and this introduces a tiny amount of overhead\r\n               (a call into a function pointer) because the calls to 'debug'\r\n               cannot be inlined.\r\n\r\n  Default: False\r\n\r\n\r\nFlag network-uri\r\n  Description: Get Network.URI from the network-uri package\r\n  Default: True\r\n\r\n\r\nLibrary\r\n  Default-language:  Haskell2010\r\n  hs-source-dirs: src\r\n\r\n  if !flag(debug)\r\n    cpp-options: -DNODEBUG\r\n\r\n  if flag(portable) || os(windows)\r\n    cpp-options: -DPORTABLE\r\n    build-depends: time-locale-compat == 0.1.*\r\n  else\r\n    c-sources: cbits/timefuncs.c\r\n    include-dirs: cbits\r\n    build-depends: old-locale >= 1 && <2\r\n\r\n  exposed-modules:\r\n    Snap.Core,\r\n    Snap.Internal.Core,\r\n    Snap.Internal.Debug,\r\n    Snap.Internal.Http.Types,\r\n    Snap.Internal.Parsing,\r\n    Snap.Internal.Util.FileServe,\r\n    Snap.Test,\r\n    Snap.Types.Headers,\r\n    Snap.Util.CORS,\r\n    Snap.Util.FileServe,\r\n    Snap.Util.FileUploads,\r\n    Snap.Util.GZip,\r\n    Snap.Util.Proxy\r\n\r\n  other-modules:\r\n    Snap.Internal.Instances,\r\n    Snap.Internal.Routing,\r\n    Snap.Internal.Test.RequestBuilder,\r\n    Snap.Internal.Test.Assertions,\r\n    Snap.Internal.Util.FileUploads\r\n\r\n\r\n  build-depends:\r\n    HUnit                     >= 1.2     && < 2,\r\n    attoparsec                >= 0.12    && < 0.15,\r\n    base                      >= 4       && < 5,\r\n    bytestring                >= 0.9     && < 0.12,\r\n    bytestring-builder        >= 0.10.4  && < 0.11,\r\n    case-insensitive          >= 1.1     && < 1.3,\r\n    containers                >= 0.3     && < 1.0,\r\n    directory                 >= 1       && < 2,\r\n    filepath                  >= 1.1     && < 2.0,\r\n    lifted-base               >= 0.1     && < 0.3,\r\n    io-streams                >= 1.3     && < 1.6,\r\n    hashable                  >= 1.2.0.6 && < 1.5,\r\n    monad-control             >= 1.0     && < 1.1,\r\n    mtl                       >= 2.0     && < 2.3,\r\n    random                    >= 1       && < 2,\r\n    readable                  >= 0.1     && < 0.4,\r\n    regex-posix               >= 0.95    && < 1,\r\n    text                      >= 0.11    && < 2.1,\r\n    time                      >= 1.0     && < 1.14,\r\n    transformers              >= 0.3     && < 0.6,\r\n    transformers-base         >= 0.4     && < 0.5,\r\n    unix-compat               >= 0.3     && < 0.7,\r\n      -- allow unix-compat-0.6, see https://github.com/snapframework/snap-core/pull/317\r\n    unordered-containers      >= 0.1.4.3 && < 0.3,\r\n    vector                    >= 0.6     && < 0.13\r\n\r\n  other-extensions:\r\n    BangPatterns,\r\n    CPP,\r\n    DeriveDataTypeable,\r\n    EmptyDataDecls,\r\n    ExistentialQuantification,\r\n    FlexibleContexts,\r\n    FlexibleInstances,\r\n    ForeignFunctionInterface,\r\n    GeneralizedNewtypeDeriving,\r\n    MultiParamTypeClasses,\r\n    OverloadedStrings,\r\n    OverloadedStrings,\r\n    PackageImports,\r\n    Rank2Types,\r\n    ScopedTypeVariables,\r\n    TypeSynonymInstances\r\n\r\n  if impl(ghc >= 6.12.0)\r\n    ghc-options: -Wall -fwarn-tabs -fno-warn-unused-do-bind\r\n  else\r\n    ghc-options: -Wall -fwarn-tabs\r\n\r\n  -- See https://ghc.haskell.org/trac/ghc/wiki/Migration/8.0#base-4.9.0.0\r\n\r\n  if impl(ghc < 8.8)\r\n    if impl(ghc >= 8.0)\r\n      ghc-options: -Wcompat -Wnoncanonical-monad-instances -Wnoncanonical-monadfail-instances\r\n    else\r\n      build-depends: fail == 4.9.*, semigroups >= 0.18 && < 0.20\r\n\r\n  if flag(network-uri)\r\n    -- Leaving network-uri-2.7.0.0 out for now because it is marked deprecated\r\n    build-depends: network-uri >= 2.6 && < 2.7,\r\n                   network     >= 2.6 && < 3.2\r\n  else\r\n    build-depends: network-uri >= 2.5 && < 2.6,\r\n                   network     >= 2.3 && < 2.6\r\n\r\n  if impl(ghc >= 7.6)\r\n    build-depends: unix-compat >= 0.3\r\n  else\r\n    build-depends: unix-compat >= 0.3 && < 0.5.3\r\n\r\n\r\nTest-suite testsuite\r\n  hs-source-dirs: src test\r\n  Type:              exitcode-stdio-1.0\r\n  Main-is:           TestSuite.hs\r\n  Default-language:  Haskell2010\r\n\r\n  if flag(portable) || os(windows)\r\n    cpp-options: -DPORTABLE\r\n    build-depends: time-locale-compat == 0.1.*\r\n  else\r\n    c-sources: cbits/timefuncs.c\r\n    include-dirs: cbits\r\n    build-depends: old-locale >= 1 && <2\r\n\r\n  other-modules:\r\n    Snap.Core,\r\n    Snap.Internal.Debug,\r\n    Snap.Internal.Http.Types,\r\n    Snap.Internal.Parsing,\r\n    Snap.Test,\r\n    Snap.Types.Headers,\r\n    Snap.Util.CORS,\r\n    Snap.Util.FileServe,\r\n    Snap.Util.FileUploads,\r\n    Snap.Util.GZip,\r\n    Snap.Util.Proxy,\r\n    Snap.Internal.Core,\r\n    Snap.Internal.Instances,\r\n    Snap.Internal.Routing,\r\n    Snap.Internal.Test.RequestBuilder,\r\n    Snap.Internal.Test.Assertions,\r\n    Snap.Internal.Util.FileServe,\r\n    Snap.Internal.Util.FileUploads,\r\n    --------------------------------------------------------------------------\r\n    Snap.Core.Tests,\r\n    Snap.Internal.Http.Types.Tests,\r\n    Snap.Internal.Parsing.Tests,\r\n    Snap.Internal.Routing.Tests,\r\n    Snap.Test.Common,\r\n    Snap.Test.Tests,\r\n    Snap.Types.Headers.Tests,\r\n    Snap.Util.CORS.Tests,\r\n    Snap.Util.FileServe.Tests,\r\n    Snap.Util.FileUploads.Tests,\r\n    Snap.Util.GZip.Tests,\r\n    Snap.Util.Proxy.Tests\r\n\r\n  build-depends:\r\n    HUnit,\r\n    attoparsec,\r\n    base,\r\n    bytestring,\r\n    bytestring-builder,\r\n    case-insensitive,\r\n    containers,\r\n    directory,\r\n    filepath,\r\n    hashable,\r\n    lifted-base,\r\n    io-streams,\r\n    monad-control,\r\n    mtl,\r\n    random,\r\n    readable,\r\n    regex-posix,\r\n    text,\r\n    time,\r\n    transformers,\r\n    transformers-base,\r\n    unix-compat,\r\n    unordered-containers,\r\n    vector,\r\n    --------------------------------------------------------------------------\r\n    QuickCheck                 >= 2.3.0.2  && <3,\r\n    deepseq                    >= 1.1      && < 1.5,\r\n    parallel                   >= 3        && <4,\r\n    test-framework             >= 0.8.0.3  && <0.9,\r\n    test-framework-hunit       >= 0.2.7    && <0.4,\r\n    test-framework-quickcheck2 >= 0.2.12.1 && <0.4,\r\n    zlib                       >= 0.5      && <0.7\r\n\r\n  if flag(network-uri)\r\n    build-depends: network-uri,\r\n                   network\r\n  else\r\n    build-depends: network-uri,\r\n                   network\r\n\r\n  ghc-options: -Wall -fwarn-tabs -funbox-strict-fields -threaded\r\n               -fno-warn-unused-do-bind\r\n\r\n  -- See https://ghc.haskell.org/trac/ghc/wiki/Migration/8.0#base-4.9.0.0\r\n  if impl(ghc < 8.8)\r\n    if impl(ghc >= 8.0)\r\n      ghc-options: -Wcompat -Wnoncanonical-monad-instances -Wnoncanonical-monadfail-instances\r\n    else\r\n      build-depends: fail == 4.9.*, semigroups >= 0.18 && < 0.20\r\n\r\n  other-extensions:\r\n    BangPatterns,\r\n    CPP,\r\n    DeriveDataTypeable,\r\n    EmptyDataDecls,\r\n    ExistentialQuantification,\r\n    FlexibleContexts,\r\n    FlexibleInstances,\r\n    ForeignFunctionInterface,\r\n    GeneralizedNewtypeDeriving,\r\n    MagicHash,\r\n    MultiParamTypeClasses,\r\n    OverloadedStrings,\r\n    Rank2Types,\r\n    ScopedTypeVariables,\r\n    StandaloneDeriving,\r\n    TypeFamilies,\r\n    TypeSynonymInstances\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: git://github.com/snapframework/snap-core.git\r\n";
    }