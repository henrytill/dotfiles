{
  allowUnfree = true;

  packageOverrides = super: let self = super.pkgs; in {

    haskell =
      let
        lib = super.haskell.lib;
      in
      super.haskell // {
        packages = super.haskell.packages // {
          ghc802 = super.haskell.packages.ghc802.override {
            overrides = self: super: {
              process-extras = lib.dontCheck super.process-extras;
            };
          };
        };
      };

    haskellPackages =
      let
        lib = super.haskell.lib;
      in
      super.haskellPackages.override {
        overrides = self: super: {};
      };

    htScripts = super.recurseIntoAttrs (super.callPackage ./pkgs/htScripts.nix {});
    htShells  = super.recurseIntoAttrs (super.callPackage ./pkgs/htShells.nix {});
    weechat   = super.callPackage ./pkgs/weechat-minimal.nix {};
  };
}
