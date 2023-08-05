let pkgs = import ./nixpkgs.nix;
    my-cabal-install = (import ./cabalinstall.nix) pkgs;
    haskellPackages = pkgs.haskell.packages.ghc962;

in haskellPackages.shellFor {
  packages = hpkgs: [];

  # development tools we use
  nativeBuildInputs = with pkgs; [
    my-cabal-install
    ghc
    haskell-language-server
    ghcid
    nixfmt
    git
    openssh
    coreutils
    nano
    less
  ];

  shellHook = ''
    export PATH=$PATH:${pkgs.haskell-language-server}/bin
  '';

}