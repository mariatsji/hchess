let
    pkgs = import (builtins.fetchTarball {
        url = "https://github.com/NixOS/nixpkgs/archive/c00039f697caa02208a01e7712bfe11c484cceca.tar.gz";
    }) {};

in pkgs