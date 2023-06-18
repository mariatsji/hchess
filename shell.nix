let pkgs = import ./nixpkgs.nix;

in pkgs.haskellPackages.shellFor {
  packages = hpkgs: [];

  # development tools we use
  nativeBuildInputs = with pkgs; [
    cabal-install
    ghc
    haskell-language-server
    ghcid
    haskellPackages.fourmolu
    nixfmt
    git
    openssh
    coreutils
  ];

  shellHook = ''
    export PATH=$PATH:${pkgs.haskell-language-server}/bin
  '';

}