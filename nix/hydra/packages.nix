# A set of buildables we typically build for releases

{ self, ... }: {
  perSystem = { pkgs, lib, asZip, hsPkgs, ... }: {
    packages =
      let
        # Creates a fixed length string by padding with given filler as suffix.
        padSuffix = width: filler: str:
          let
            strw = lib.stringLength str;
            reqWidth = width - (lib.stringLength filler);
          in
          assert lib.assertMsg (strw <= width)
            "padSuffix: requested string length (${toString width}) must not be shorter than actual length (${toString strw})";
          if strw == width then str else padSuffix reqWidth filler str + filler;

        paddedRevision = padSuffix 40 " " (self.rev or "dirty");

        # Placeholder used to 'embedRevision'. See also hydra-prelude/cbits/revision.c
        placeholder = "0000000001000000000100000000010000000001";

        # Takes a derivation with a single executable and embeds the given revision
        # into this executable.
        embedRevision = drv: exe: rev:
          assert lib.assertMsg
            (lib.stringLength placeholder == lib.stringLength rev)
            "Mismatching length of placeholder (${placeholder}) and rev (${rev})";

          # Using mkDerivation instead of runCommand to make sure to use the same
          # stdenv as the original drv (important to determine targetPlatform).
          drv.stdenv.mkDerivation {
            name = "${exe}";
            buildInputs = pkgs.lib.optionals pkgs.stdenv.isDarwin [ pkgs.cctools ];
            phases = [ "buildPhase" ] ++ pkgs.lib.optionals pkgs.stdenv.isDarwin [ "postFixup" ];
            buildPhase = ''
              set -e

              echo "Patching embedded git revision in ${exe} to ${rev} ..."

              # Ensure only one occurrence of placeholder
              if [[ $(grep -c -a ${placeholder} ${drv}/bin/${exe}) -ne 1 ]]; then
                echo "Not exactly one occurrence of ${placeholder} in ${drv}/bin/${exe}!"
                exit 1
              fi

              mkdir -p $out/bin
              sed 's/${placeholder}/${rev}/' ${drv}/bin/${exe} > $out/bin/${exe}
              chmod +x $out/bin/${exe}
            '';
            postFixup = pkgs.lib.optionalString pkgs.stdenv.isDarwin ''
              install_name_tool -add_rpath ${pkgs.zlib}/lib $out/bin/${exe}
              install_name_tool -add_rpath ${pkgs.lmdb}/lib $out/bin/${exe}
              install_name_tool -add_rpath ${pkgs.libcxx}/lib $out/bin/${exe}
              install_name_tool -add_rpath ${pkgs.libiconv}/lib $out/bin/${exe}
              install_name_tool -add_rpath ${pkgs.libffi}/lib $out/bin/${exe}
            '';
          };

        nativePkgs = hsPkgs;
        # Allow reinstallation of terminfo as it's not installed with cross compilers.
        patchedForCrossProject = hsPkgs.appendModule
          ({ lib, ... }: { options.nonReinstallablePkgs = lib.mkOption { apply = lib.remove "terminfo"; }; });
        musl64Pkgs = patchedForCrossProject.projectCross.musl64.hsPkgs;
      in
      rec {
        release =
          asZip
            { name = "hydra-${pkgs.hostPlatform.system}"; }
            [ hydra-node hydra-tui ];

        release-static =
          asZip
            { name = "hydra-${pkgs.hostPlatform.system}"; }
            [ hydra-node-static hydra-tui-static ];

        hydra-node =
          embedRevision
            nativePkgs.hydra-node.components.exes.hydra-node
            "hydra-node"
            paddedRevision;

        hydra-node-static =
          embedRevision
            musl64Pkgs.hydra-node.components.exes.hydra-node
            "hydra-node"
            paddedRevision;

        hydra-chain-observer =
          embedRevision
            nativePkgs.hydra-chain-observer.components.exes.hydra-chain-observer
            "hydra-chain-observer"
            paddedRevision;

        hydra-chain-observer-static =
          embedRevision
            musl64Pkgs.hydra-chain-observer.components.exes.hydra-chain-observer
            "hydra-chain-observer"
            paddedRevision;

        hydra-cluster = pkgs.writers.writeBashBin "hydra-cluster" ''
          export PATH=$PATH:${hydra-node}/bin
          ${nativePkgs.hydra-cluster.components.exes.hydra-cluster}/bin/hydra-cluster "$@"
        '';

        inherit (nativePkgs.hydra-node.components.benchmarks) tx-cost;

        hydra-tui =
          embedRevision
            nativePkgs.hydra-tui.components.exes.hydra-tui
            "hydra-tui"
            paddedRevision;

        hydra-tui-static =
          embedRevision
            musl64Pkgs.hydra-tui.components.exes.hydra-tui
            "hydra-tui"
            paddedRevision;

        inherit (nativePkgs.hydraw.components.exes) hydraw;

        hydraw-static = musl64Pkgs.hydraw.components.exes.hydraw;

        hydra-plutus-tests = pkgs.mkShellNoCC {
          name = "hydra-plutus-tests";
          buildInputs = [
            nativePkgs.hydra-plutus.components.tests.tests
            pkgs.aiken
          ];
        };
        hydra-tx-tests = pkgs.mkShellNoCC {
          name = "hydra-tx-tests";
          buildInputs = [
            nativePkgs.hydra-tx.components.tests.tests
          ];
        };
        hydra-chain-observer-tests = pkgs.mkShellNoCC {
          name = "hydra-chain-observer-tests";
          buildInputs = [
            nativePkgs.hydra-chain-observer.components.tests.tests
          ];
        };
        hydra-node-tests = pkgs.mkShellNoCC {
          name = "hydra-node-tests";
          buildInputs = [
            nativePkgs.hydra-node.components.tests.tests
            pkgs.check-jsonschema
          ];
        };
        hydra-cluster-tests = pkgs.mkShellNoCC {
          name = "hydra-cluster-tests";
          buildInputs =
            [
              nativePkgs.hydra-cluster.components.tests.tests
              hydra-node
              hydra-chain-observer
              pkgs.cardano-node
              pkgs.cardano-cli
              pkgs.mithril-client-cli
              pkgs.check-jsonschema
            ];
        };
        hydra-tui-tests = pkgs.mkShellNoCC {
          name = "hydra-tui-tests";
          buildInputs =
            [
              nativePkgs.hydra-tui.components.tests.tests
              hydra-node
              pkgs.cardano-node
              pkgs.cardano-cli
            ];
        };

        hydra-node-bench = pkgs.mkShellNoCC {
          name = "hydra-node-bench";
          buildInputs = [
            nativePkgs.hydra-node.components.benchmarks.tx-cost
            nativePkgs.hydra-node.components.benchmarks.micro
          ];
        };
        hydra-cluster-bench = pkgs.mkShellNoCC {
          name = "hydra-cluster-bench";
          buildInputs =
            [
              nativePkgs.hydra-cluster.components.benchmarks.bench-e2e
              hydra-node
              pkgs.cardano-node
              pkgs.cardano-cli
            ] ++ pkgs.lib.optionals pkgs.stdenv.isLinux [
              pkgs.dool
            ];
        };

        haddocks = pkgs.runCommand "hydra-haddocks"
          {
            paths = [
              hsPkgs.hydra-prelude.components.library.doc
              hsPkgs.hydra-cardano-api.components.library.doc
              hsPkgs.hydra-plutus.components.library.doc
              hsPkgs.hydra-node.components.library.doc
              hsPkgs.hydra-tx.components.library.doc
              hsPkgs.hydra-tx.components.tests.tests.doc
              hsPkgs.hydra-node.components.tests.tests.doc
              hsPkgs.hydra-cluster.components.library.doc
              hsPkgs.hydra-tui.components.library.doc
            ];
          }
          ''
            set -ex
            mkdir -p $out
            for p in $paths; do
              cd $p
              for html in $(find $haddockRoot -name html -type d); do
                package=$(basename $(dirname $html))
                mkdir -p $out/$package
                cp -a $html/* $out/$package/
              done
            done
          '';
      };
  };
}
