pkgsFinal: pkgsPrev:

import ./lib/override-haskell-packages.nix {
  packages = {
    "starring" = pkgsPrev.nix-gitignore.gitignoreSource [ ../.nixignore ] ../.;
  };
  overrides = {
    "ki" = _: { broken = false; };
    "starring" = _: { doBenchmark = true; };
  };
} pkgsFinal pkgsPrev
