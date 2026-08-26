_: {

  perSystem = { config, pkgs, pkgs-2511, ... }:
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

      # The Typst render, WITHOUT the notation-tooltip postprocess
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
          config.packages.spec-typst
        ];
        meta = { };
        src = specSrc;
        # build.sh renders the literate-Typst sources with Typst (no
        # LaTeX/Inkscape toolchain needed). --ignore-system-fonts keeps Typst
        # reproducible: only the fonts bundled with Typst plus JuliaMono from
        # nixpkgs (code blocks, wired through JULIAMONO_FONT_DIR, see build.sh)
        # are used.
        JULIAMONO_FONT_DIR = juliaMonoFontDir;
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
      # Typst with the spec's diagram packages, reused by the spec build and
      # exposed so the dev shell can offer the same `typst` for working on the spec.
      packages.spec-typst = spec-typst;

      # The python environment annotate-notation.py needs, exposed for the same
      # reason: the shell and the build stamp tooltips with one interpreter.
      packages.spec-python = spec-python;

      # Everything needed to run `spec/build.sh` by hand (`just spec`). Kept out
      # of the default dev shell so developers who never touch the spec do not
      # download the typst wrapper and a second python closure.
      devShells.spec = pkgs.mkShell {
        name = "hydra-spec-shell";
        buildInputs = [
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
        buildPhase = ''
          python3 ${specSrc}/annotate-notation.py \
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
    };
}
