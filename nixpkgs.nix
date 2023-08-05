let
    pkgs = import (builtins.fetchTarball {
        url = "https://github.com/NixOS/nixpkgs/archive/d75ba3d7a2b5c4952f89f498e435cb5648c0f0cb.tar.gz";
    }) {};

in pkgs
