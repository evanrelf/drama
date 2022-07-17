{
  description = "drama";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    haskell-overlay.url = "github:evanrelf/haskell-overlay";
    nixpkgs.url = "github:NixOS/nixpkgs";
  };

  outputs = inputs@{ flake-utils, nixpkgs, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            inputs.haskell-overlay.overlay
            (pkgsFinal: pkgsPrev:
              let
                inherit (pkgsPrev) haskell-overlay;
              in
              haskell-overlay.mkOverlay
                {
                  extensions = [
                    (haskell-overlay.sources (_: _: {
                      ki = "1.0.0";
                    }))
                  ];
                }
                pkgsFinal
                pkgsPrev
            )
          ];
        };

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
