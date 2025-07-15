{ inputs, self, ... }: {
  perSystem = { config, system, compiler, ... }:
    let
      pkgs = import inputs.nixpkgs {
        inherit system;
        overlays = [
          # This overlay contains libsodium and libblst libraries
          inputs.iohk-nix.overlays.crypto
          # This overlay contains pkg-config mappings via haskell.nix to use the
          # crypto libraries above
          inputs.iohk-nix.overlays.haskell-nix-crypto
          # Keep haskell.nix as the last overlay!
          #
          # Reason: haskell.nix modules/overlays needs to be last
          # https://github.com/input-output-hk/haskell.nix/issues/1954
          inputs.haskellNix.overlay
          # Custom static libs used for darwin build
          self.overlays.static-libs
          inputs.nix-npm-buildpackage.overlays.default
          # Specific versions of tools we require
          (_final: _prev: {
            inherit (inputs.aiken.packages.${system}) aiken;
            apply-refact = pkgs.haskell-nix.tool compiler "apply-refact" "0.15.0.0";
            cabal-install = pkgs.haskell-nix.tool compiler "cabal-install" "3.10.3.0";
            cabal-plan = pkgs.haskell-nix.tool compiler "cabal-plan" { version = "0.7.5.0"; cabalProjectLocal = "package cabal-plan\nflags: +exe"; };
            cabal-fmt = config.treefmt.programs.cabal-fmt.package;
            fourmolu = config.treefmt.programs.fourmolu.package;
            haskell-language-server = pkgs.haskell-nix.tool compiler "haskell-language-server" "2.11.0.0";
            weeder = pkgs.haskell-nix.tool compiler "weeder" "2.9.0";
            inherit (inputs.cardano-node.packages.${system}) cardano-cli;
            inherit (inputs.cardano-node.packages.${system}) cardano-node;
            inherit (inputs.mithril.packages.${system}) mithril-client-cli;
          })
        ];
      };
    in
    {
      _module.args = { inherit pkgs; };
    };

}
