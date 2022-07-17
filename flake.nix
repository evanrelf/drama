{
  description = "drama";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs";
  };

  outputs = inputs@{ flake-utils, nixpkgs, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

      in
      rec {
        packages = {
          default = packages.drama;

          drama = pkgs.haskellPackages.callCabal2nix "drama" ./. { };
        };

        devShells = {
          default = devShells.drama;

          drama =
            packages.drama.env.overrideAttrs (prev: {
              buildInputs = (prev.buildInputs or [ ]) ++ [
                pkgs.cabal-install
                pkgs.ghcid
              ];
            });
        };
      }
    );
}
