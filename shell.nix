{ pkgs ? import ./nix/pkgs.nix { inherit ghcVersion; }
, ghcVersion ? null
}:

pkgs.haskellPackages.shellFor {
  packages = p: [
    (pkgs.haskell.lib.doBenchmark p.drama)
  ];

  buildInputs = [
    pkgs.cabal-install
    pkgs.ghcid
    pkgs.haskellPackages.stan
    pkgs.hlint
  ];

  doBenchmark = true;

  withHoogle = true;
}
