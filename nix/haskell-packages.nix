pkgsFinal: pkgsPrev:

import ./lib/override-haskell-packages.nix {
  packages = {
    "drama" = pkgsPrev.nix-gitignore.gitignoreSource [ ../.nixignore ] ../.;
  };

  overrideCabal = {
    "ki" = _: { broken = false; };
  };

  overrideAttrs = {
    "drama" = old: {
      outputs = (old.outputs or []) ++ [ "hie" ];
      postBuild = (old.postBuild or "") + ''
        cp -R .hie $hie
      '';
    };
  };
} pkgsFinal pkgsPrev
