{ haskellPackages, stdenv }:

let
  drama = haskellPackages.drama;

  configFile = builtins.path {
    name = "stan.toml";
    path = ../.stan.toml;
  };

in
  stdenv.mkDerivation {
    name = "${drama.name}-stan";

    src = drama.src;

    nativeBuildInputs = [ haskellPackages.stan ];

    phases = [ "unpackPhase" "checkPhase" ];

    doCheck = true;

    checkPhase = ''
      stan --hiedir "${drama.hie}" --config-file ${configFile}
      touch "$out"
    '';
  }
