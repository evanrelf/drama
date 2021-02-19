let
  pkgs = import ./nix/pkgs.nix;

  drama = pkgs.haskellPackages.drama;

  drama-hlint = pkgs.callPackage ./hlint.nix {};

  drama-stan = pkgs.callPackage ./stan.nix {};

in {
  inherit
    drama
    drama-hlint
    drama-stan
  ;
}
