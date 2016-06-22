{
  allowUnfree = true;

  packageOverrides = super: let self = super.pkgs; in {
    htShells = self.recurseIntoAttrs (self.callPackage ./pkgs/htShells.nix {});
  };
}
