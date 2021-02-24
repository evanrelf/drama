{ ghcVersion ? null }:

import ./nixpkgs.nix {
  overlays = [
    (import ./haskell-packages.nix)
    (pkgsFinal: pkgsPrev: { inherit ghcVersion; })
  ];
}
