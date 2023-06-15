{
  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs";
    haskellNix.url = "github:ch1bo/haskell.nix?ref=enable-haddock-tests";
    iohk-nix.url = "github:input-output-hk/iohk-nix";
    flake-utils.url = "github:numtide/flake-utils";
    CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };
    cardano-node.url = "github:input-output-hk/cardano-node/1.35.7";
  };

  outputs =
    { self
    , flake-utils
    , nixpkgs
    , cardano-node
    , ...
    } @ inputs:
    flake-utils.lib.eachSystem [
      "x86_64-linux"
      "x86_64-darwin"
      # XXX: Disabled until cardano-node releases a verison supporting this
      # "aarch64-darwin"
    ]
      (system:
      let
        pkgs = import inputs.nixpkgs { inherit system; };
        hydraProject = import ./nix/hydra/project.nix {
          inherit (inputs) haskellNix iohk-nix CHaP;
          inherit system nixpkgs;
          gitRev = self.rev or "dirty";
        };
        hydraPackages = import ./nix/hydra/packages.nix {
          inherit hydraProject system pkgs cardano-node;
        };
        hydraImages = import ./nix/hydra/docker.nix {
          inherit hydraPackages system nixpkgs;
        };
        prefixAttrs = s: attrs:
          with pkgs.lib.attrsets;
          mapAttrs' (name: value: nameValuePair (s + name) value) attrs;
        haddocks = pkgs.runCommand "hydra-haddocks"
          {
            paths = [
              hydraProject.hsPkgs.plutus-cbor.components.library.doc
              hydraProject.hsPkgs.plutus-merkle-tree.components.library.doc
              hydraProject.hsPkgs.hydra-prelude.components.library.doc
              hydraProject.hsPkgs.hydra-cardano-api.components.library.doc
              hydraProject.hsPkgs.hydra-plutus.components.library.doc
              hydraProject.hsPkgs.hydra-node.components.library.doc
              hydraProject.hsPkgs.hydra-node.components.tests.tests.doc
              hydraProject.hsPkgs.hydra-cluster.components.library.doc
              hydraProject.hsPkgs.hydra-tui.components.library.doc
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
      in
      rec {
        inherit hydraProject;

        packages =
          hydraPackages //
          prefixAttrs "docker-" hydraImages // {
            spec = import ./spec { inherit pkgs; };
          } // {
            inherit haddocks;
          };

        devShells = (import ./nix/hydra/shell.nix {
          inherit (inputs) cardano-node;
          inherit hydraProject system;
        }) // {
          ci = (import ./nix/hydra/shell.nix {
            inherit (inputs) cardano-node;
            inherit hydraProject system;
            withoutDevTools = true;
          }).default;
        };

        # Build selected derivations in CI for caching
        hydraJobs = {
          packages = { inherit (packages) hydra-node hydra-tui hydraw spec; };
          devShells = { inherit (devShells) default ci; };
        };
      });

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://hydra-node.cachix.org"
      "https://cardano-scaling.cachix.org"
      "https://cache.zw3rk.com"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "hydra-node.cachix.org-1:vK4mOEQDQKl9FTbq76NjOuNaRD4pZLxi1yri31HHmIw="
      "cardano-scaling.cachix.org-1:RKvHKhGs/b6CBDqzKbDk0Rv6sod2kPSXLwPzcUQg9lY="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    ];
    allow-import-from-derivation = true;
  };
}
