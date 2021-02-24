{ ghcVersion ? null
, packages ? {}
, override ? {}
, overrideCabal ? {}
, overrideAttrs ? {}
, hackage ? null
}:

pkgsFinal: pkgsPrev:

let
  packagesExtension = pkgsPrev.haskell.lib.packageSourceOverrides packages;

  overrideExtension = haskellPackagesFinal: haskellPackagesPrev:
    let
      applyOverride = name: fn: haskellPackagesPrev."${name}".override fn;
    in
      pkgsPrev.lib.mapAttrs applyOverride override;

  overrideCabalExtension = haskellPackagesFinal: haskellPackagesPrev:
    let
      applyOverride = name: fn:
        pkgsPrev.haskell.lib.overrideCabal haskellPackagesPrev."${name}" fn;
    in
      pkgsPrev.lib.mapAttrs applyOverride overrideCabal;

  overrideAttrsExtension = haskellPackagesFinal: haskellPackagesPrev:
    let
      applyOverride = name: fn: haskellPackagesPrev."${name}".overrideAttrs fn;
    in
      pkgsPrev.lib.mapAttrs applyOverride overrideAttrs;

  haskellPackages =
    (if ghcVersion == null
       then pkgsPrev.haskellPackages
       else pkgsPrev.haskell.packages."${ghcVersion}"
    ).override (old: {
      overrides =
        pkgsPrev.lib.fold
          pkgsPrev.lib.composeExtensions
          (old.overrides or (_: _: {}))
          [ packagesExtension
            overrideExtension
            overrideCabalExtension
            overrideAttrsExtension
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
