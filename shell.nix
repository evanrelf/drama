let
  pkgs = import ./nix/pkgs.nix;

  # These are defined as a separate scripts, instead of Bash functions in
  # `shellHook`, for compatibility with `lorri` (`lorri` doesn't support
  # `shellHook`).

  hoogle-open = pkgs.writeShellScriptBin "hoogle-open" ''
    #!/usr/bin/env bash

    set -Eeuo pipefail
    IFS=$'\n\t'

    if command -v xdg-open >/dev/null 2>&1; then
      open="xdg-open"
    elif command -v open >/dev/null 2>&1; then
      open="open"
    else
      echo "Missing `open` or `xdg-open`" >&2
      exit 1
    fi

    if [ "$#" -eq 0 ]; then
      "$open" "http://localhost:9999"
      command hoogle server --port 9999 --local
    else
      command hoogle "$@"
    fi
  '';

  haddock-open = pkgs.writeShellScriptBin "haddock-open" ''
    #!/usr/bin/env bash

    set -Eeuo pipefail
    IFS=$'\n\t'

    if command -v xdg-open >/dev/null 2>&1; then
      open="xdg-open"
    elif command -v open >/dev/null 2>&1; then
      open="open"
    else
      echo "Missing `open` or `xdg-open`" >&2
      exit 1
    fi

    cabal haddock -O0 | tee /dev/stderr | tail -n 1 | xargs "$open"
  '';

in
  pkgs.haskellPackages.shellFor {
    packages = p: [
      (pkgs.haskell.lib.doBenchmark p.drama)
    ];

    buildInputs = [
      haddock-open
      hoogle-open
      pkgs.cabal-install
      pkgs.ghcid
      pkgs.haskellPackages.stan
      pkgs.hlint
    ];

    doBenchmark = true;

    withHoogle = true;

    shellHook = ''
      alias hoogle=hoogle-open
    '';
  }
