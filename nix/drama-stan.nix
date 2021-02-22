{ haskellPackages, stdenv }:

let
  drama = haskellPackages.drama;

in
  stdenv.mkDerivation {
    name = "${drama.name}-stan";

    src = drama.src;

    nativeBuildInputs = [ haskellPackages.stan ];

    phases = [ "unpackPhase" "checkPhase" ];

    doCheck = true;

    checkPhase = ''
      stan --hiedir "${drama.hie}"
    '';
  }
