{ mkDerivation, fetchFromGitHub, standard-library, standard-library-classes }:

mkDerivation rec {
  version = "2.1.1";
  pname = "agda-stdlib-meta";

  src = fetchFromGitHub {
    owner = "agda";
    repo = "agda-stdlib-meta";
    rev = "v${version}";
    sha256 = "sha256-qOoThYMG0dzjKvwmzzVZmGcerfb++MApbaGRzLEq3/4=";
  };

  buildInputs = [ standard-library standard-library-classes ];

  libraryFile = "agda-stdlib-meta.agda-lib";
  everythingFile = "Main.agda";

  meta = { };
}

