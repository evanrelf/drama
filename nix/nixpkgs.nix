args:

let
  # master on 2021-02-11
  rev = "0f3b7ea1dd1605c5a7049ce596f9f24e72f52a0f";
  sha256 = "15mzdjq43rh5r71z3838jg7lk7jzymxzavlvc7c7nkfcvsw3bgr5";

  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };

in
  import nixpkgs ({ config = {}; } // args)
