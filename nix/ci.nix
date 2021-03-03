let
  checkout = {
    name = "Checkout";
    uses = "actions/checkout@v2.3.4";
  };

  installNix = {
    name = "Install Nix";
    uses = "cachix/install-nix-action@v12";
    "with" = {
      nix_path = "nixpkgs=./nix/pkgs.nix";
    };
  };

  setUpCachix = {
    name = "Set up Cachix";
    uses = "cachix/cachix-action@v8";
    "with" = {
      name = "evanrelf";
      authToken = "\${{ secrets.CACHIX_AUTH_TOKEN }}";
    };
  };

  makeJob = command: {
    runs-on = "ubuntu-latest";
    steps = [
      checkout
      installNix
      setUpCachix
      { name = "nix-build"; run = command; }
    ];
  };

in {
  name = "ci";

  on = {
    push.branches = [ "main" ];
    pull_request = {};
  };

  jobs = {
    check-ci-config = {
      runs-on = "ubuntu-latest";
      steps = [
        { run = "./scripts/generate-ci-json && git diff --exit-code"; }
      ];
    };

    build-ghc865 =
      makeJob "nix-build --attr drama --argstr ghcVersion 'ghc865'";

    build-ghc884 =
      makeJob "nix-build --attr drama --argstr ghcVersion 'ghc884'";

    build-ghc8103 =
      makeJob "nix-build";
  };
}
