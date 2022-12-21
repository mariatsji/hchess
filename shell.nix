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
            haskellPackages.hp2pretty
        ];
    tools = with pkgs;
        [ 
            nixfmt
            git
            curl
        ];
    ux = with pkgs;[ gtk4 atk ];
    
    all = haskellStuff ++ tools ++ ux;


in pkgs.mkShell {
  # specify which packages to add to the shell environment
  packages = all;
  libPath = pkgs.lib.makeLibraryPath all;
  shellHook = ''
        export LD_LIBRARY_PATH=$libPath}:$LD_LIBRARY_PATH
        export LANG=en_US.UTF-8
    '';
  # add all the dependencies, of the given packages, to the shell environment
  inputsFrom = with pkgs; all;
}