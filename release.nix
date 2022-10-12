let pkgs = import ./nixpkgs.nix;
    hchess = pkgs.haskellPackages.callCabal2nix "hchess" ./. {};

in pkgs.haskell.lib.overrideCabal hchess (old: {
  enableSharedExecutables = false;
  enableSharedLibraries = false;
  configureFlags = [ ];
})