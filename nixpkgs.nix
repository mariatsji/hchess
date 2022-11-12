let
    pkgs = import (builtins.fetchTarball {
        url = "https://github.com/NixOS/nixpkgs/archive/4d5f61d6c7edf1eede542ede24ce752741e7819c.tar.gz";
    }) {};

in pkgs