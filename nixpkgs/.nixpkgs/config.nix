{
  allowUnfree = true;

  packageOverrides = super: let self = super.pkgs; in {
    giter8 = super.callPackage ./pkgs/giter8 {};
    racket-minimal = super.callPackage ./pkgs/racket/minimal.nix {};
  };
}
