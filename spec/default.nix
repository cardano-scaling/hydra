{ pkgs ? import <nixpkgs> { }
}:
let
  buildScript = ./build.sh;
in
pkgs.stdenv.mkDerivation rec {
  name = "hydra-spec";
  src = ./.;
  buildInputs = with pkgs; [
    pandoc
    haskellPackages.pandoc-crossref
    texlive.combined.scheme-small
    nodePackages.mermaid-cli # mmdc
    librsvg # rsvg-convert
  ];
  phases = [ "unpackPhase" "buildPhase" ];
  buildPhase = ''
    ${buildScript} $out
  '';
}
