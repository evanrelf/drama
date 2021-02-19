let
  pkgs = import ./nix/pkgs.nix;

  starring = pkgs.haskellPackages.starring;

in
  starring.env.overrideAttrs (old: {
    buildInputs = with pkgs; old.buildInputs ++ [
      cabal-install
      ghcid
    ];
  })
