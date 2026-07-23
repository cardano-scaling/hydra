{ self, inputs, ... }: {

  perSystem = { config, pkgs, pkgs-2411, pkgs-2511, ... }:
    let
      inherit (pkgs) lib;

      # The @preview diagram packages, pinned here and supplied to the wrapped
      # typst via TYPST_PACKAGE_CACHE_PATH so the build stays hermetic without
      # vendoring them into the repo. The same versions appear in the sources'
      # import strings, so both are derived from this one attribute set: a
      # version that no longer matches the imports fails evaluation with the
      # message below, instead of silently sending `nix build` and the dev shell
      # to different package versions.
      typstPackageVersions =
        let
          diagramsSource = builtins.readFile ../../spec/src/diagrams.typ;
        in
        lib.mapAttrs
          (name: version:
            if lib.hasInfix "@preview/${name}:${version}" diagramsSource
            then version
            else
              throw ''
                nix/hydra/spec.nix pins @preview/${name}:${version}, but
                spec/src/diagrams.typ imports a different version of ${name}.
                Bump both, or drop the pin if the import is gone.
              '')
          {
            cetz = "0.3.4";
            fletcher = "0.5.8";
          };

      # nixpkgs names these `<name>_<version with dots as underscores>`.
      typstPackage = p: name: version: p.${"${name}_${lib.replaceStrings [ "." ] [ "_" ] version}"};

      # typst >= 0.14.1 (0.14.0 emits the PDF named-destination name tree
      # unsorted, typst#7248, killing internal section links in viewers that
      # binary-search it per spec: pdf.js, PDFium, macOS Preview). This is the
      # only thing the 25.11 pin is needed for, see flake.nix.
      spec-typst = pkgs-2511.typst.withPackages (p:
        lib.mapAttrsToList (typstPackage p) typstPackageVersions
        # oxifmt is a transitive dependency of cetz, so it has no import string
        # of its own to keep in lockstep.
        ++ [ p.oxifmt_0_2_1 ]);

      # The specification's Agda libraries, pinned to the 24.11 Agda (the
      # formal-ledger / abstract-set-theory set is built against it).
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

      # PyMuPDF for annotate-notation.py. Deliberately from the default pin, not
      # 25.11: keeping typst the sole consumer of that input is what makes the
      # "drop when the main pin ships typst >= 0.14.1" note in flake.nix true.
      spec-python = pkgs.python3.withPackages (ps: [ ps.pymupdf ]);

      # JuliaMono is the code font (see template.typ); typst only warns on a
      # missing family, so build.sh hard-errors when this is unset.
      juliaMonoFontDir = "${pkgs.julia-mono}/share/fonts/truetype";

      # Just the spec/ tree, so the derivation hash tracks the spec sources
      # instead of the whole flake: with `${self}/spec` any commit anywhere (a
      # Haskell module, a README) changed `self` and invalidated this
      # minutes-long render, making `checks.spec` a guaranteed cache miss on
      # every PR.
      specSrc = lib.fileset.toSource {
        root = ../../spec;
        fileset = ../../spec;
      };

      # The Agda typecheck + lints + Typst render, WITHOUT the notation-tooltip postprocess
      # (ANNOTATE_NOTATION=skip, see build.sh stage 3). Internal: consume
      # packages.spec, which adds the tooltips.
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
          config.packages.spec-typst
        ];
        meta = { };
        src = specSrc;
        # build.sh typechecks the literate-Typst sources with Agda and renders
        # the PDF with Typst (no LaTeX/Inkscape toolchain needed).
        # --ignore-system-fonts keeps Typst
        # reproducible: only the fonts bundled with Typst plus JuliaMono from
        # nixpkgs (code blocks, wired through JULIAMONO_FONT_DIR, see build.sh)
        # are used.
        JULIAMONO_FONT_DIR = juliaMonoFontDir;
        ANNOTATE_NOTATION = "skip";
        buildPhase = ''
          export HOME=$TMPDIR
          # Never trust a _build/ that arrived with the sources. build.sh short-circuits when the PDF
          # is newer than every input, which is a developer convenience with no place in a hermetic
          # build: nix normalises every mtime to the epoch, so a committed PDF is never "older" than
          # the sources, and the only thing standing between a stray committed _build/ and a check
          # that installs that PDF without typechecking anything is the recorded-mode comparison.
          # That is too thin a thread for the gate the whole formalisation hangs from.
          rm -rf _build
          bash build.sh
        '';
        installPhase = ''
          mkdir $out
          cp _build/hydra-spec.pdf $out/hydra-spec.pdf
        '';
      };
    in
    {
      # Typst with the spec's diagram packages, reused by the spec build and
      # exposed so the dev shell can offer the same `typst` for working on the spec.
      packages.spec-typst = spec-typst;

      # Agda with the specification's libraries, reused by the spec build and
      # exposed so the dev shell can offer the same `agda` for working on the spec.
      packages.spec-agda = agdaPackages.withPackages agdaLibraries;

      # The python environment annotate-notation.py needs, exposed for the same
      # reason: the shell and the build stamp tooltips with one interpreter.
      packages.spec-python = spec-python;

      # Everything needed to run `spec/build.sh` by hand (`just spec`). Kept out
      # of the default dev shell so developers who never touch the spec do not
      # download the typst wrapper and a second python closure.
      devShells.spec = pkgs.mkShell {
        name = "hydra-spec-shell";
        buildInputs = [
          config.packages.spec-agda
          config.packages.spec-typst
          config.packages.spec-python
        ];
        JULIAMONO_FONT_DIR = juliaMonoFontDir;
      };

      # The publishable spec PDF: the render above plus the notation hover
      # tooltips (build.sh stage 3, split out - see the spec-rendered comment).
      packages.spec = pkgs.stdenv.mkDerivation {
        pname = "hydra-spec.pdf";
        version = "0.0.1";
        nativeBuildInputs = [
          # for annotate-notation.py (stamps the tooltips, needs PyMuPDF)
          config.packages.spec-python
        ];
        meta = { };
        dontUnpack = true;
        # Call the interpreter by its absolute store path rather than resolving `python3` on PATH.
        # That environment is this derivation's ONLY input, yet on the aarch64-darwin builders the
        # stage died with `No module named 'pymupdf'` AND `No module named 'fitz'`, which cannot be
        # the environment: the buildPhase running at all means the env built, the env cannot build
        # unless pymupdf did, and pymupdf's own install check imports both names (doInstallCheck is
        # on for darwin too). So the name resolved to some other interpreter, which the darwin
        # sandbox's system paths make possible and an absolute path makes impossible.
        #
        # The import probe stays as a guard: this stage has now failed twice on a platform we cannot
        # build locally, and it turns "ModuleNotFoundError" into a report naming the interpreter and
        # its search path, which is the difference between diagnosing the next occurrence and
        # guessing at it again.
        buildPhase = ''
          python3=${config.packages.spec-python}/bin/python3
          "$python3" -c 'import pymupdf' || {
            echo "annotate stage: $python3 cannot import pymupdf; it reports:"
            "$python3" -c 'import sys; print("executable:", sys.executable); print("path:", sys.path)'
            exit 1
          }
          "$python3" ${specSrc}/annotate-notation.py \
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
