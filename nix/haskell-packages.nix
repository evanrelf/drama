pkgsFinal: pkgsPrev:

import ./lib/override-haskell-packages.nix {
  packages = {
    "drama" = pkgsPrev.nix-gitignore.gitignoreSource [ ../.nixignore ] ../.;
  };
  overrides = {
    "drama" = _: { doBenchmark = true; };
    "ki" = _: { broken = false; };
  };
} pkgsFinal pkgsPrev
