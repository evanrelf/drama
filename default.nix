let
  pkgs = import ./nix/pkgs.nix;

  drama = pkgs.haskellPackages.drama;

  drama-hlint = pkgs.callPackage ./nix/drama-hlint.nix {};

  drama-stan = pkgs.callPackage ./nix/drama-stan.nix {};

in {
  inherit
    drama
    drama-hlint
    drama-stan
  ;
}
