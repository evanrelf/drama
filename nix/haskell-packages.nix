pkgsFinal: pkgsPrev:

import ./lib/override-haskell-packages.nix {
  ghcVersion = pkgsFinal.ghcVersion;

  packages = {
    "drama" = pkgsPrev.nix-gitignore.gitignoreSource [ ../.nixignore ] ../.;
  };

  overrideCabal = {
    "ki" = _: { broken = false; };
  };
} pkgsFinal pkgsPrev
