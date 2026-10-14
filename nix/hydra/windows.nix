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
              ${pkgs.buildPackages.python3}/bin/python ${../../scripts/prepare-windows-scripts.py} src
            '';
          })
        ];
      });
      windowsProject = project.projectCross.ucrt64;
      windowsNode = pkgs.runCommand "hydra-node-windows" { } ''
        ${pkgs.python3}/bin/python ${../../scripts/package-windows.py} \
          ${windowsProject.hsPkgs.hydra-node.components.exes.hydra-node}/bin \
          "$out/bin" ${lib.escapeShellArg (self.rev or "dirty")} \
          ${pkgs.pkgsCross.ucrt64.stdenv.cc.bintools}/bin/x86_64-w64-mingw32-objdump
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
          zip -9 "$out/hydra-x86_64-windows.zip" ./*
        '';
      };
    };
}
