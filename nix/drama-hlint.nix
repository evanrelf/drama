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
      hlint . --report
      mkdir "$out"
      mv report.html "$out/hlint.html"
    '';
  }
