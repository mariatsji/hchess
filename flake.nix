{
  description = "hChess";

  inputs = {
      nixpkgs = ./nixpkgs.nix;
      #  sha256 = "sha256:02k1lpjg7yz1h87i6wjv332djz06z8kqcg8mmkxp923kvx52d23m";
      
  };

  outputs = { self, nixpkgs }: {

    packages.x86_64-darwin.hchess = import ./release.nix;
  };
}
