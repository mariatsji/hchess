let
    pkgs = import (builtins.fetchTarball {
        url = "https://github.com/NixOS/nixpkgs/archive/d75ba3d7a2b5c4952f89f498e435cb5648c0f0cb.tar.gz";
        sha256 = "sha256:0d23zmjm352id4pii0qfy4x9fdp36f13rz5pcf6rz5r7zd9figzg";
    }) {};

in pkgs
