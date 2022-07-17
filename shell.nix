{ pkgs ? import ./nix/pkgs.nix { inherit ghcVersion; }
, ghcVersion ? null
}:

pkgs.haskellPackages.drama.env.overrideAttrs (prev: {
  buildInputs = (prev.buildInputs or [ ]) ++ [
    pkgs.cabal-install
    pkgs.ghcid
  ];
})
