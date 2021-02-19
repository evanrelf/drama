{ haskellPackages, hlint, stdenv }:

let
  drama = haskellPackages.drama;

in
  stdenv.mkDerivation {
    name = "${drama.pname}-hlint";

    src = drama.src;

    nativeBuildInputs = [ hlint ];

    phases = [ "unpackPhase" "checkPhase" ];

    doCheck = true;

    checkPhase = ''
      hlint .
      touch "$out"
    '';
  }
