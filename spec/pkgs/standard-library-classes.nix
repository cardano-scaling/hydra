{ mkDerivation, fetchFromGitHub, standard-library }:

mkDerivation rec {
  version = "2.0";
  pname = "standard-library-classes";

  src = fetchFromGitHub {
    owner = "omelkonian";
    repo = "agda-stdlib-classes";
    rev = "28df278381c94a25c54f6819524cd9f8cb99f092";
    sha256 = "sha256-TdPJ3K4jyAIQgX1sUrqd0QeA72n2mkBVzlg8WfrqWWY=";
  };

  buildInputs = [ standard-library ];

  libraryFile = "agda-stdlib-classes.agda-lib";
  everythingFile = "standard-library-classes.agda";

  meta = { };
}

