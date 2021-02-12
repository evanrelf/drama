let
  pkgs = import ./nix/nixpkgs.nix {};

  actress = import ./default.nix;

in
  actress.env.overrideAttrs (old: {
    buildInputs = with pkgs; old.buildInputs ++ [
      cabal-install
      ghcid
    ];
  })
