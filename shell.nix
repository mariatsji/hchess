let pkgs = import ./nixpkgs.nix;
    my-cabal-install = (import ./cabalinstall.nix) pkgs;
    my-haskell-language-server = (builtins.getFlake "github:haskell/haskell-language-server").allPackages.x86_64-darwin.haskell-language-server-96;
    haskellPackages = pkgs.haskell.packages.ghc962;

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