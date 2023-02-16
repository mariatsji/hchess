{
  description = "hChess";

  outputs = { self, nixpkgs }: {

    packages.x86_64-linux.hchess = import ./release.nix;

    packages.x86_64-linux.default = self.packages.x86_64-linux.hello;

  };
}
