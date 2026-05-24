{ inputs, ... }: {

  imports = [
    inputs.hydra-coding-standards.flakeModule
    inputs.process-compose-flake.flakeModule
  ];

  perSystem = { pkgs, hsPkgs, system, ... }:
    let
      compiler = "ghc9124";
      # For GHC 9.12 we replace Hackage's proto-lens with a master-branch source
      # that has the necessary GHC 9.12 fixes. The repo uses git submodules and
      # symlinks; cabal's source-repository-package fetch cannot follow either,
      # so we fetch with submodules in Nix and materialise the symlinks before
      # haskell.nix hands the source to cabal. Mirrors cardano-node 11.0.1's
      # nix/haskell.nix workaround.
      protoLensSrc = pkgs.fetchgit {
        url = "https://github.com/google/proto-lens";
        rev = "20de5227947b0c37dd6852dcc6f2db1cd5889cee";
        hash = "sha256-VUYU2swjU7L8Zdu6Zfz6jo2ulW5uPhAamt2GjH5hZRY=";
        fetchSubmodules = true;
      };
      fixProtoLensSrc = pkgs.runCommand "proto-lens-fixed" { } ''
        mkdir -p $out
        cp -a ${protoLensSrc}/. $out/
        chmod -R +w $out
        rm -rf $out/proto-lens/proto-lens-imports/google
        cp -r ${protoLensSrc}/google/protobuf/src/google $out/proto-lens/proto-lens-imports/
        rm -rf $out/proto-lens-protobuf-types/proto-src
        cp -r ${protoLensSrc}/google/protobuf/src $out/proto-lens-protobuf-types/proto-src
        chmod -R -w $out
      '';
      inputMap = {
        "https://intersectmbo.github.io/cardano-haskell-packages" = inputs.CHaP;
        "https://github.com/google/proto-lens/20de5227947b0c37dd6852dcc6f2db1cd5889cee" = fixProtoLensSrc;
      };
      pkgs-2411 = import inputs.nixpkgs-2411 { inherit system; };
    in
    {
      _module.args = { inherit compiler inputMap pkgs-2411; };
      legacyPackages = pkgs // hsPkgs;
    };
}
