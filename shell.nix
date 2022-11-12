{ pkgs ? import ./nixpkgs.nix }:

let hax = pkgs.haskellPackages.override {
        overrides = self: super: rec {
        wx = pkgs.haskell.lib.dontCheck
            (self.callHackage "wx" "0.92.3.0" { });
        wxdirect = pkgs.haskell.lib.dontCheck
            (self.callHackage "wxdirect" "0.92.3.0" { });
        process = pkgs.haskell.lib.dontCheck
            (self.callHackage "process" "1.4.3.0" { });
        #process = pkgs.haskell.lib.dontCheck
        #    (self.callHackage "process" )
        # stm-containers = pkgs.haskell.lib.dontCheck
        #  (self.callHackage "stm-containers" "1.1.0.4" { });
        }; 
    };
    haskellStuff = with pkgs;
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
    ux = with pkgs;[ gtk3 pkgconfig gobject-introspection ];
    
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