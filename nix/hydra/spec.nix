{ self, inputs, ... }: {

  perSystem = { pkgs, pkgs-2411, ... }:
    let
      agdaPackages = pkgs-2411.callPackage "${self}/spec/pkgs/initial-packages.nix" {
        inherit (pkgs-2411.haskellPackages) Agda;
        nixpkgs = inputs.nixpkgs-2411;
      };
      agdaLibraries = with agdaPackages; [
        abstract-set-theory
        formal-ledger
        standard-library
        standard-library-classes
        standard-library-meta
      ];
    in
    {
      packages.spec = agdaPackages.mkDerivation {
        pname = "hydra-spec.pdf";
        version = "0.0.1";
        nativeBuildInputs = with pkgs; [
          (agdaPackages.withPackages agdaLibraries)
          (haskellPackages.ghcWithPackages (p: [ p.shake ]))
          inkscape
          texlive.combined.scheme-full
        ];
        meta = { };
        src = "${self}/spec";
        buildPhase = "shake";
        installPhase = ''
          mkdir $out
          cp _build/hydra-spec.pdf $out/hydra-spec.pdf
        '';
      };
    };
}
