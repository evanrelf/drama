{ haskellPackages, stdenv }:

let
  drama = haskellPackages.drama;

in
  stdenv.mkDerivation {
    name = "${drama.pname}-stan";

    src = drama.src;

    nativeBuildInputs = [ haskellPackages.stan ];

    phases = [ "unpackPhase" "checkPhase" ];

    doCheck = true;

    checkPhase = ''
      stan --hiedir "${drama.hie}" report
      mkdir "$out"
      mv stan.html "$out"
    '';
  }
