pkgsFinal: pkgsPrev:

import ./lib/override-haskell-packages.nix {
  packages = {
    "drama" = pkgsPrev.nix-gitignore.gitignoreSource [ ../.nixignore ] ../.;
  };
  overrides = {
    "ki" = _: { broken = false; };
  };
} pkgsFinal pkgsPrev
