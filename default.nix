let
  pkgs = import ./nix/pkgs.nix;

in
  pkgs.haskellPackages.drama.overrideAttrs (old: {
    nativeBuildInputs = (old.nativeBuildInputs or []) ++ [
      pkgs.haskellPackages.hlint
      pkgs.haskellPackages.stan
    ];

    checkPhase = (old.checkPhase or "") + ''
      echo "Running HLint"
      hlint .

      echo "Running Stan"
      stan
    '';
  })
