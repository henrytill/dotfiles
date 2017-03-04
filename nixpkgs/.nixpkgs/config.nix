{
  allowUnfree = true;

  packageOverrides = super: let self = super.pkgs; in {

    haskell = super.haskell // {
      packages = super.haskell.packages // {
        ghc802 = super.haskell.packages.ghc802.override {
          overrides = self: super: {
            process-extras = self.callPackage ./pkgs/haskell/process-extras-0.7.1.nix {};
          };
        };
      };
    };

    htShells = self.recurseIntoAttrs (self.callPackage ./pkgs/htShells.nix {});
  };
}
