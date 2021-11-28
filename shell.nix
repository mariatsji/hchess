let nixpkgs = import (fetchTarball https://github.com/nixos/nixpkgs/archive/071317d543205ee5f5611d391a37582f9b282240.tar.gz) {};

in nixpkgs.haskell.lib.buildStackProject {
    LANG = "en_US.UTF-8";
    ghc = nixpkgs.haskell.compiler.ghc8107;
    name = "hchess";
    buildInputs = [
        # Build-time system dependencies.
        nixpkgs.cacert
        nixpkgs.coreutils
        nixpkgs.findutils
        nixpkgs.gnutar
        nixpkgs.zlib
        nixpkgs.libiconv
    ];
    src = if nixpkgs.lib.inNixShell then null else ./.; # Only needed for nix-build, and causes problems for nix-shell during local development.
}