{ pkgs ? import <nixpkgs> { }
}:

pkgs.stdenvNoCC.mkDerivation rec {
  name = "hydra-spec";
  src = ./.;
  buildInputs = [
    pkgs.texlive.combined.scheme-full
  ];
  phases = [ "unpackPhase" "buildPhase" ];
  buildPhase = ''
    pdflatex -file-line-error --synctex=1 -interaction=nonstopmode ${./main.tex}
    mkdir -p $out
    mv *-main.pdf $out/${name}.pdf
  '';
}
