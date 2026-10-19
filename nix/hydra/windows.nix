# Cross-build the node on Linux; no Windows development environment is required.
{ self, inputs, ... }: {
  perSystem = { pkgs, lib, system, compiler, inputMap, ... }:
    let
      etcdWindows = pkgs.etcd_3_6.deps.etcdserver.overrideAttrs (old: {
        pname = "etcd-windows";
        env = old.env // { GOOS = "windows"; GOARCH = "amd64"; };
        doCheck = false;
        dontStrip = true;
        preInstall = ''
          mv "$GOPATH/bin/windows_amd64/server.exe" "$GOPATH/bin/etcd.exe"
          rmdir "$GOPATH/bin/windows_amd64"
        '';
      });
      nativeScripts =
        let ghc = project.ghcWithPackages (hs: [ hs.hydra-plutus ]); in
        pkgs.runCommand "hydra-plutus-scripts" { } ''
          flatUnit=$(${ghc}/bin/ghc-pkg field z-plutus-core-z-flat id --simple-output)
          ${ghc}/bin/ghc -XGHC2021 -package-id "$flatUnit (PlutusCore.Flat)" \
            -outputdir . ${./ExportScripts.hs} -o export-scripts
          ./export-scripts "$out"
        '';
      project = pkgs.haskell-nix.cabalProject' ({ pkgs, ... }: {
        src = pkgs.haskell-nix.haskellLib.cleanSourceWith {
          name = "hydra-windows";
          src = self;
        };
        compiler-nix-name = compiler;
        inherit inputMap;
        # Exclude executables such as the Unix-only TUI from dependency solving.
        # Keep the shared repository pins and local libraries in one place.
        cabalProject = builtins.replaceStrings
          [ "  **/*.cabal" ]
          [
            (lib.concatMapStringsSep "\n" (p: "  ${p}/${p}.cabal") [
              "hydra-node"
              "hydra-cardano-api"
              "hydra-plutus"
              "hydra-plutus-extras"
              "hydra-prelude"
              "hydra-test-utils"
              "hydra-tx"
            ])
          ]
          (builtins.readFile ../../cabal.project);
        configureArgs = "--disable-tests --disable-benchmarks";
        cabalProjectLocal = ''
          constraints: any.Cabal installed
        '' + lib.optionalString pkgs.stdenv.hostPlatform.isWindows ''
          package hydra-plutus
            flags: +precompiled-scripts
          package bitvec
            flags: -simd
        '';
        modules = [
          {
            reinstallableLibGhc = false;
            doHaddock = false;
          }
          ({ pkgs, ... }: {
            packages.proto-lens-protobuf-types.components.library.build-tools = [ pkgs.buildPackages.protobuf ];
            packages.proto-lens-etcd.components.library.build-tools = [ pkgs.buildPackages.protobuf ];
          })
          ({ pkgs, lib, ... }: lib.mkIf pkgs.stdenv.hostPlatform.isWindows {
            # network >= 3.2.8 sets an unsupported option when creating IPv4 sockets.
            # https://github.com/haskell/network/issues/608
            packages.network.patches = [ ./network-windows-sockets.patch ];
            # hsc2hs represents INVALID_HANDLE_VALUE as an integer constant.
            # GCC 14 rejects its implicit pointer conversion by default.
            packages.basement.configureFlags = [ "--hsc2hs-option=--cflag=-Wno-int-conversion" ];
            # The pinned haskell.nix patch reference omits its .patch suffix.
            packages.crypton-x509-system.patches = lib.mkForce [
              "${inputs.haskellNix}/overlays/patches/crypton-x509-system.patch"
            ];
            # Wine exposes the Linux filesystem through drive Z for TH IO.
            packages.hydra-node.components.library.preBuild = ''
              export HYDRA_ETCD_PATH="Z:${etcdWindows}/bin/etcd.exe"
            '';
            # The Plutus plugin cannot load into this cross-compiler. Compile
            # the scripts natively from the same sources and dependency pins.
            packages.hydra-plutus.preConfigure = ''
              cp -r ${nativeScripts} cross-scripts
            '';
          })
        ];
      });
      windowsProject = project.projectCross.ucrt64;
      runtime = pkgs.pkgsCross.ucrt64;
      # Take notices from the exact pinned sources, not from Nix's sometimes
      # broader package-level license metadata (notably GCC and mcfgthread).
      runtimePackage = { package, attribute, license, licenseFiles, upstream }: {
        inherit attribute license licenseFiles upstream;
        pname = package.pname or (lib.getName package);
        inherit (package) version;
        output = toString (lib.getLib package);
        source = toString package.src;
        sourceUrls = package.src.urls or [ ];
        sourceRevision = package.src.rev or null;
        sourceHash = package.src.outputHash or (package.src.narHash or null);
        sourceHashMode = package.src.outputHashMode or "recursive";
      };
      buildInput = repository: input: {
        revision = input.rev;
        sourceHash = input.narHash;
        url = "https://github.com/${repository}/tree/${input.rev}";
      };
      runtimeManifest = pkgs.writeText "windows-runtime.json" (builtins.toJSON {
        packages = map runtimePackage [
          {
            package = runtime.stdenv.cc.cc;
            attribute = "stdenv.cc.cc.lib";
            license = "GPL-3.0-or-later WITH GCC-exception-3.1";
            licenseFiles = [ "COPYING" "COPYING3" "COPYING.LIB" "COPYING3.LIB" "COPYING.RUNTIME" ];
            upstream = "https://gcc.gnu.org/";
          }
          {
            package = runtime.gmp;
            attribute = "gmp";
            license = "LGPL-3.0-or-later OR GPL-2.0-or-later";
            licenseFiles = [ "COPYING" "COPYINGv2" "COPYINGv3" "COPYING.LESSERv3" ];
            upstream = "https://gmplib.org/";
          }
          {
            package = runtime.windows.mcfgthreads;
            attribute = "windows.mcfgthreads";
            license = "LGPL-3.0-or-later WITH GCC-exception-3.1";
            licenseFiles = [ "LICENSE.TXT" "licenses" ];
            upstream = "https://github.com/lhmouse/mcfgthread";
          }
          {
            package = runtime.libffi;
            attribute = "libffi";
            license = "MIT";
            licenseFiles = [ "LICENSE" ];
            upstream = "https://github.com/libffi/libffi";
          }
          {
            package = runtime.libsodium-vrf;
            attribute = "libsodium-vrf";
            license = "ISC";
            licenseFiles = [ "LICENSE" ];
            upstream = "https://github.com/input-output-hk/libsodium";
          }
          {
            package = runtime.secp256k1;
            attribute = "secp256k1";
            license = "MIT";
            licenseFiles = [ "COPYING" ];
            upstream = "https://github.com/bitcoin-core/secp256k1";
          }
          {
            package = runtime.snappy;
            attribute = "snappy";
            license = "BSD-3-Clause";
            licenseFiles = [ "COPYING" ];
            upstream = "https://github.com/google/snappy";
          }
          {
            package = runtime.zlib;
            attribute = "zlib";
            license = "Zlib";
            licenseFiles = [ "LICENSE" ];
            upstream = "https://github.com/madler/zlib";
          }
        ];
        # MinGW code is also linked into the executables and DLLs themselves.
        mingw = runtimePackage {
          package = runtime.windows.mingw_w64;
          attribute = "windows.mingw_w64";
          license = "See the accompanying MinGW-w64 runtime notices";
          licenseFiles = [ "COPYING" "COPYING.MinGW-w64-runtime" ];
          upstream = "https://www.mingw-w64.org/";
        };
        buildInputs = {
          nixpkgs = buildInput "NixOS/nixpkgs" inputs.nixpkgs;
          haskellNix = buildInput "input-output-hk/haskell.nix" inputs.haskellNix;
          iohk-nix = buildInput "input-output-hk/iohk-nix" inputs.iohk-nix;
        };
      });
      windowsNode = pkgs.runCommand "hydra-node-windows" { } ''
        ${pkgs.python3}/bin/python ${../../scripts/package-windows.py} \
          ${windowsProject.hsPkgs.hydra-node.components.exes.hydra-node}/bin \
          "$out/bin" ${lib.escapeShellArg (self.rev or "dirty")} \
          ${runtime.stdenv.cc.bintools}/bin/x86_64-w64-mingw32-objdump \
          ${runtimeManifest} ${self}
      '';
    in
    {
      packages = lib.optionalAttrs (system == "x86_64-linux") {
        hydra-node-windows = windowsNode;
        release-windows = pkgs.runCommand "hydra-x86_64-windows.zip"
          {
            nativeBuildInputs = [ pkgs.zip ];
          } ''
          mkdir -p "$out"
          cp ${nativeScripts}/linux-script-catalogue.json "$out/"
          cd ${windowsNode}/bin
          zip -9 -r "$out/hydra-x86_64-windows.zip" ./*
        '';
      };
    };
}
