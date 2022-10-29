let
    pkgs = import (builtins.fetchTarball {
        url = "https://github.com/NixOS/nixpkgs/archive/323786dc2b28bc80b7f3970ec69b4f32b01aead6.tar.gz";
    }) {};

in pkgs