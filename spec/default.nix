{ pkgs }:

pkgs.stdenvNoCC.mkDerivation rec {
  name = "hydra-spec";
  src = ./.;
  buildInputs = [
    pkgs.texlive.combined.scheme-full
  ];
  phases = [ "unpackPhase" "buildPhase" ];
  buildPhase = ''
    latexmk -pdf ${./main.tex}
    mkdir -p $out
    mv *-main.pdf $out/${name}.pdf
  '';
}
