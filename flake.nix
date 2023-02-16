{
  description = "hChess";

  outputs = { self, nixpkgs }: {

    packages.x86_64-darwin.hchess = import ./release.nix;

    packages.x86_64-darwin.default = self.packages.x86_64-darwin.hchess;

  };
}
