{ haskellPackages, stdenv }:

let
  drama = haskellPackages.drama.overrideAttrs (old: {
    outputs = (old.outputs or []) ++ [ "hie" ];
    postBuild = (old.postBuild or "") + ''
      cp -R .hie $hie
    '';
  });

in
  stdenv.mkDerivation {
    name = "${drama.name}-stan";

    src = drama.src;

    nativeBuildInputs = [ haskellPackages.stan ];

    phases = [ "unpackPhase" "checkPhase" ];

    doCheck = true;

    checkPhase = ''
      stan --hiedir "${drama.hie}"
      touch "$out"
    '';
  }
