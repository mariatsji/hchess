let
    pkgs = import (builtins.fetchTarball {
        url = "https://github.com/NixOS/nixpkgs/archive/4d5f61d6c7edf1eede542ede24ce752741e7819c.tar.gz";
        sha256 = "sha256:02k1lpjg7yz1h87i6wjv332djz06z8kqcg8mmkxp923kvx52d23m";
    }) {};

in pkgs