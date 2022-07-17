args:

let
  # master on 2022-07-16
  rev = "ff0e4cfbcd7881f50caa788ebd488e2e6f782431";
  sha256 = "1s0gx9ikh36h2w6pj9jjll7i32hzzmzmrax9w9jf405rd7822qx3";

  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };

in
  import nixpkgs ({ config = {}; } // args)
