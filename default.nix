let
  haskellPackagesOverlay =
    import ./nix/override-haskell-packages.nix {
      packages = {
        "actress" = pkgs.nix-gitignore.gitignoreSource [ ./.nixignore ] ./.;
      };
      overrides = {
        "ki" = _: { broken = false; };
      };
    };

  pkgs = import ./nix/nixpkgs.nix { overlays = [ haskellPackagesOverlay ]; };

in
  pkgs.haskellPackages.actress
