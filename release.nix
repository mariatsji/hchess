let
  pkgs = (import (builtins.fetchTarball {
        url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/24.05.tar.gz";
    }) {});
  haskellPackages = pkgs.haskell.packages.ghc962;
  hchess = haskellPackages.callCabal2nix "hchess-gui" ./. { };
  thinner = x:
    with pkgs;
    haskell.lib.disableLibraryProfiling
    (haskell.lib.dontHaddock (haskell.lib.dontCheck x));
  hchessThin = thinner hchess;

in pkgs.haskell.lib.overrideCabal hchessThin (old: {
  enableSharedExecutables = false;
  enableSharedLibraries = false;
  configureFlags = [ ];
})
