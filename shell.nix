let
  pkgs = import ./nix/nixpkgs.nix {};

  starring = import ./default.nix;

in
  starring.env.overrideAttrs (old: {
    buildInputs = with pkgs; old.buildInputs ++ [
      cabal-install
      ghcid
    ];
  })
