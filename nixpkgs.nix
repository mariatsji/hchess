let
    pkgs = import (builtins.fetchTarball {
        url = "https://github.com/NixOS/nixpkgs/archive/88bdb6d79b0bdf03d3338f6f3d1416a55ec199ab.tar.gz.tar.gz";
        sha256 = "sha256:02k1lpjg7yz1h87i6wjv332djz06z8kqcg8mmkxp923kvx52d23m";
    }) {};

in pkgs