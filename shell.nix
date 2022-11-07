{ pkgs ? import ./nixpkgs.nix }:

let haskellStuff = with pkgs;
        [ 
            haskellPackages.haskell-language-server
            ghc
            haskellPackages.cabal-install
            haskellPackages.cabal2nix
            haskellPackages.implicit-hie
            ghcid
            haskellPackages.fourmolu
        ];
    tools = with pkgs;
        [ 
            nixfmt
            git
            curl
        ];
    graphics = with pkgs; [ haskellPackages.haskell-gi-base haskellPackages.gi-gtk ]; # pkg-config glib gobject-introspection ];
    all = haskellStuff ++ tools ++ graphics;


in pkgs.mkShell {
  # specify which packages to add to the shell environment
  packages = all;
  # add all the dependencies, of the given packages, to the shell environment
  inputsFrom = with pkgs; all;
}