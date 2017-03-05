{
  allowUnfree = true;

  packageOverrides = super: let self = super.pkgs; in {

    haskellPackages = super.haskellPackages.override {
      overrides = self: super: {
        process-extras = self.callPackage ./pkgs/haskell/process-extras-0.7.1.nix {};
      };
    };

    htShells = self.recurseIntoAttrs (self.callPackage ./pkgs/htShells.nix {});
  };
}
