let
  pkgs = import ./nixpkgs.nix;
  hchess = pkgs.haskellPackages.callCabal2nix "hchess" ./. { };
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
