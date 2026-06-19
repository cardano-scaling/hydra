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
      # Agda with the specification's libraries, reused by the spec build and
      # exposed so the dev shell can offer the same `agda` for working on the spec.
      packages.spec-agda = agdaPackages.withPackages agdaLibraries;

      packages.spec = pkgs.stdenv.mkDerivation {
        pname = "hydra-spec.pdf";
        version = "0.0.1";
        nativeBuildInputs = with pkgs; [
          (agdaPackages.withPackages agdaLibraries)
          typst
        ];
        meta = { };
        src = "${self}/spec";
        # build.sh typechecks the literate-Typst sources with Agda, stages them,
        # and renders the PDF with Typst (no LaTeX/Inkscape toolchain needed).
        # --ignore-system-fonts keeps Typst reproducible: only the bundled fonts
        # and the vendored src/fonts (StrippedJuliaMono) are used.
        buildPhase = ''
          export HOME=$TMPDIR
          bash build.sh
        '';
        installPhase = ''
          mkdir $out
          cp _build/hydra-spec.pdf $out/hydra-spec.pdf
        '';
      };
    };
}
