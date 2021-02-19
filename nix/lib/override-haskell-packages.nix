{ packages ? {}  # Add or replace Haskell packages
, overrides ? {} # Override existing Haskell packages
, hackage ? null # Specify revision of all-cabal-hashes
}:

pkgsFinal: pkgsPrev:

let
  packagesExtension = pkgsPrev.haskell.lib.packageSourceOverrides packages;

  overridesExtension = haskellPackagesFinal: haskellPackagesPrev:
    let
      applyOverride = name: fn:
        pkgsPrev.haskell.lib.overrideCabal haskellPackagesPrev."${name}" fn;
    in
      pkgsPrev.lib.mapAttrs applyOverride overrides;

  haskellPackages =
    pkgsPrev.haskellPackages.override (old: {
      overrides =
        pkgsPrev.lib.fold
          pkgsPrev.lib.composeExtensions
          (old.overrides or (_: _: {}))
          [ packagesExtension
            overridesExtension
          ];
    });

  all-cabal-hashes =
    if hackage == null then
      pkgsPrev.all-cabal-hashes
    else
      builtins.trace
        ''
          Ignore warnings about using `fetchFromGitHub` for archives from GitHub!
          The `fetchFromGitHub` utility hangs when fetching `commercialhaskell/all-cabal-hashes`.
        ''
        (pkgsPrev.fetchurl {
          url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/${hackage.rev}.tar.gz";
          sha256 = hackage.sha256;
        });

in
  { inherit
      haskellPackages
      all-cabal-hashes
    ;
  }
