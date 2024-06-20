let pkgs = (import (builtins.fetchTarball {
        url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/24.05.tar.gz";
    }) {});
    my-cabal-install = pkgs.cabal-install;
    my-haskell-language-server = pkgs.haskell-language-server;
    haskellPackages = pkgs.haskellPackages;

in haskellPackages.shellFor {
  packages = hpkgs: [];

  # development tools we use
  nativeBuildInputs = with pkgs; [
    my-cabal-install
    ghc
    my-haskell-language-server
    ghcid
    nixfmt
    git
    openssh
    coreutils
    nano
    less
    which
  ];

}