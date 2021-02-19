pkgsFinal: pkgsPrev:

import ./lib/override-haskell-packages.nix {
  packages = {
    "drama" = pkgsPrev.nix-gitignore.gitignoreSource [ ../.nixignore ] ../.;
  };

  overrideCabal = {
    "ki" = _: { broken = false; };
  };
} pkgsFinal pkgsPrev
