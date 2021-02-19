let
  pkgs = import ./nix/pkgs.nix;

  drama = pkgs.haskellPackages.drama;

  drama-hlint = pkgs.callPackage ./hlint.nix {};


in {
  inherit
    drama
    drama-hlint
  ;
}
