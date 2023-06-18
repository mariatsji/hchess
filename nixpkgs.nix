let
    pkgs = import (builtins.fetchTarball {
        url = "https://github.com/NixOS/nixpkgs/archive/88bdb6d79b0bdf03d3338f6f3d1416a55ec199ab.tar.gz";
    }) {};

in pkgs
