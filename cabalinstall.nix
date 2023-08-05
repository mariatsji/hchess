pkgs :

let
  thinner = with pkgs;
    x:
    lib.pipe x [
      haskell.lib.compose.doJailbreak
      haskell.lib.compose.dontHaddock
      haskell.lib.compose.dontCheck
      haskell.lib.compose.dontBenchmark
      haskell.lib.compose.dontCoverage
    ];
  haskellPackages = pkgs.haskell.packages.ghc962;
  cc2nix = haskellPackages.callCabal2nix;
  thinFetchSubModule = moduleName: deps:
    thinner (cc2nix moduleName ((builtins.fetchGit {
      name = moduleName;
      url = "https://github.com/haskell/cabal.git";
      rev = "30af95d67afbeda13fdfc6b7a5f558252d35a774";
      shallow = true;
    }) + "/${moduleName}") deps);

  # Cabal-syntax works!
  Cabal-syntax = thinFetchSubModule "Cabal-syntax" { };
  # Cabal works!
  Cabal = thinFetchSubModule "Cabal" { Cabal-syntax = Cabal-syntax; };
  # cabal-install-solver works!
  cabal-install-solver = thinFetchSubModule "cabal-install-solver" {
    Cabal = Cabal;
    Cabal-syntax = Cabal-syntax;
  };
  # Cabal-described works!
  Cabal-described = thinFetchSubModule "Cabal-described" {
    Cabal = Cabal;
    Cabal-syntax = Cabal-syntax;
  };
  # Cabal-tree-diff works!
  Cabal-tree-diff = thinFetchSubModule "Cabal-tree-diff" {
    Cabal = Cabal;
    Cabal-syntax = Cabal-syntax;
  };
  # works
  hackage-security-head = thinner (cc2nix "hackage-security"
    ((builtins.fetchGit {
      name = "hackage-security";
      url = "https://github.com/haskell/hackage-security.git";
      rev = "c77bfa2d78aeb3baaf3f8efa3f4fdcb8db933d0f";
      shallow = true;
    }) + "/hackage-security") { Cabal-syntax = Cabal-syntax; Cabal = Cabal; });

  directory = haskellPackages.callHackageDirect {
    pkg = "directory";
    ver = "1.3.8.1";
    sha256 = "sha256-WoQGY48ciV9+dsdDeLpjlO2PbpMAkRB0loaMMvl0e3U=";
  } { };

  cabal-install = thinFetchSubModule "cabal-install" {
    Cabal = Cabal;
    Cabal-syntax = Cabal-syntax;
    Cabal-described = Cabal-described;
    Cabal-QuickCheck = null;
    Cabal-tree-diff = Cabal-tree-diff;
    cabal-install-solver = cabal-install-solver;
    hackage-security = hackage-security-head;
  };

in cabal-install