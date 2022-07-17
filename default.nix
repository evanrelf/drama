{ pkgs ? import ./nix/pkgs.nix { inherit ghcVersion; }
, ghcVersion ? null
}:

{
  drama = pkgs.haskellPackages.drama;

  drama-stan = pkgs.callPackage ./nix/drama-stan.nix {};
}
