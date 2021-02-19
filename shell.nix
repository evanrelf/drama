let
  pkgs = import ./nix/pkgs.nix;

  # Defined as a separate script, instead of a Bash function in `shellHook`, for
  # compatibility with `lorri` (doesn't support `shellHook`s).
  hoogle-open = pkgs.writeShellScriptBin "hoogle-open" ''
    if [ "$#" -eq 0 ]; then
      open "http://localhost:9999"
      command hoogle server --port 9999 --local
    else
      command hoogle "$@"
    fi
  '';

in
  pkgs.haskellPackages.shellFor {
    packages = p: [
      (pkgs.haskell.lib.doBenchmark p.drama)
    ];

    doBenchmark = true;

    withHoogle = true;

    buildInputs = [
      hoogle-open
      pkgs.cabal-install
      pkgs.ghcid
    ];

    shellHook = ''
      alias hoogle=hoogle-open
    '';
  }
