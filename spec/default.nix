{ pkgs ? import <nixpkgs> { }
}:
let
  fonts = pkgs.makeFontsConf { fontDirectories = [ pkgs.dejavu_fonts ]; };
in
pkgs.stdenv.mkDerivation rec {
  name = "hydra-spec";
  src = ./.;
  buildInputs = with pkgs; [
    pandoc
    haskellPackages.pandoc-crossref
    texlive.combined.scheme-small
  ];
  phases = [ "unpackPhase" "buildPhase" ];
  buildPhase = ''
    export FONTCONFIG_FILE=${fonts}
    mkdir -p $out
    pandoc README.md --filter pandoc-crossref --citeproc --pdf-engine=xelatex -o $out/${name}.pdf
  '';
}
