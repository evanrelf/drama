{ pkgs ? import ./nix/pkgs.nix { inherit ghcVersion; }
, ghcVersion ? null
}:

{
  drama = pkgs.haskellPackages.drama;
}
