{ self, inputs, ... }: {

  perSystem = { config, pkgs, pkgs-2411, pkgs-2511, ... }:
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

      # The Agda typecheck + lints + Typst render, WITHOUT the notation-tooltip
      # postprocess (ANNOTATE_NOTATION=skip, see build.sh stage 3). Internal:
      # consume packages.spec, which adds the tooltips.
      #
      # The postprocess runs as the separate seconds-long derivation below so
      # this minutes-long, disk-heavy build shares no build window with the
      # python closure: a busy builder's mid-build auto-GC (observed on the
      # aarch64-darwin CI builders, where no sandbox bind-mount keeps a
      # collected path alive for a running build) once collected a late-used
      # python package out from under the final build step. Inputs are
      # re-validated when each derivation starts, so splitting shrinks the
      # exposure of the python environment from the whole render to seconds.
      spec-rendered = pkgs.stdenv.mkDerivation {
        pname = "hydra-spec-unannotated.pdf";
        version = "0.0.1";
        nativeBuildInputs = [
          config.packages.spec-agda
          # typst >= 0.14.1 (0.14.0 emits the PDF named-destination name tree
          # unsorted, typst#7248, killing internal section links in viewers
          # that binary-search it per spec: pdf.js, PDFium, macOS Preview).
          pkgs-2511.typst
        ];
        meta = { };
        src = "${self}/spec";
        # build.sh typechecks the literate-Typst sources with Agda and renders
        # the PDF with Typst (no LaTeX/Inkscape toolchain needed).
        # --ignore-system-fonts keeps Typst reproducible: only the fonts bundled
        # with Typst plus JuliaMono from nixpkgs (code blocks, wired through
        # JULIAMONO_FONT_DIR, see build.sh) are used.
        JULIAMONO_FONT_DIR = "${pkgs.julia-mono}/share/fonts/truetype";
        ANNOTATE_NOTATION = "skip";
        buildPhase = ''
          export HOME=$TMPDIR
          bash build.sh
        '';
        installPhase = ''
          mkdir $out
          cp _build/hydra-spec.pdf $out/hydra-spec.pdf
        '';
      };
    in
    {
      # Agda with the specification's libraries, reused by the spec build and
      # exposed so the dev shell can offer the same `agda` for working on the spec.
      packages.spec-agda = agdaPackages.withPackages agdaLibraries;

      # The publishable spec PDF: the render above plus the notation hover
      # tooltips (build.sh stage 3, split out - see the spec-rendered comment).
      packages.spec = pkgs.stdenv.mkDerivation {
        pname = "hydra-spec.pdf";
        version = "0.0.1";
        nativeBuildInputs = [
          # for annotate-notation.py (stamps the tooltips, needs PyMuPDF)
          (pkgs-2511.python3.withPackages (ps: [ ps.pymupdf ]))
        ];
        meta = { };
        dontUnpack = true;
        buildPhase = ''
          python3 ${self}/spec/annotate-notation.py \
            ${spec-rendered}/hydra-spec.pdf hydra-spec.pdf
        '';
        installPhase = ''
          mkdir $out
          cp hydra-spec.pdf $out/hydra-spec.pdf
        '';
      };

      # Gate the spec on PRs: `nix flake check`, `just check` (nix-fast-build over
      # .#checks) and selfci all build the flake checks. Reuses the derivation
      # above, so this adds no duplicate compilation.
      checks.spec = config.packages.spec;

      # The MAlonzo extraction under hydra-agda/generated is committed and only
      # regenerated manually (hydra-agda/regenerate.sh), so a semantic edit to
      # Reference.agda / OffChainReference.agda would otherwise silently leave the
      # committed oracle stale. Regenerate hermetically, replicating regenerate.sh
      # (same agda invocation and OPTIONS_GHC stamping), and fail on any drift.
      checks.hydra-agda-generated = pkgs.stdenv.mkDerivation {
        name = "hydra-agda-generated";
        nativeBuildInputs = [ config.packages.spec-agda ];
        src = "${self}/spec";
        buildPhase = ''
          export HOME=$TMPDIR
          agda --compile --no-main --ghc-dont-call-ghc --compile-dir="$TMPDIR/generated" \
            src/Hydra/Protocol/Reference.agda
          agda --compile --no-main --ghc-dont-call-ghc --compile-dir="$TMPDIR/generated" \
            src/Hydra/Protocol/OffChainReference.agda
          # Same `-w` stamping as regenerate.sh (see the rationale there).
          find "$TMPDIR/generated" -name '*.hs' -print0 | while IFS= read -r -d "" f; do
            chmod u+w "$f"
            if ! head -1 "$f" | grep -q -- '-w'; then
              printf '{-# OPTIONS_GHC -w #-}\n%s' "$(cat "$f")" > "$f"
            fi
          done
          diff -ru ${self}/hydra-agda/generated/MAlonzo "$TMPDIR/generated/MAlonzo" || {
            echo "hydra-agda/generated is out of date with spec/src/Hydra/Protocol/*.agda:"
            echo "run hydra-agda/regenerate.sh and commit the result."
            exit 1
          }
        '';
        installPhase = "touch $out";
      };
    };
}
