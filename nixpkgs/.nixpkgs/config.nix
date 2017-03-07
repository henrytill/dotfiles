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
        overrides = self: super: {
          purescript = super.purescript.overrideScope (self: super: {
            aeson                = super.callHackage "aeson"                "0.11.3.0" {};
            bower-json           = super.callHackage "bower-json"           "1.0.0.1"  {};
            optparse-applicative = super.callHackage "optparse-applicative" "0.13.1.0" {};
            http-client          = super.callHackage "http-client"          "0.4.31.2" {};
            http-client-tls      = super.callHackage "http-client-tls"      "0.2.4.1"  {};
            pipes                = super.callHackage "pipes"                "4.2.0"    {};
            websockets           = super.callHackage "websockets"           "0.9.8.2"  {};
          });
        };
      };

    htScripts = super.recurseIntoAttrs (super.callPackage ./pkgs/htScripts.nix {});
    htShells  = super.recurseIntoAttrs (super.callPackage ./pkgs/htShells.nix {});
    weechat   = super.callPackage ./pkgs/weechat-minimal.nix {};
  };
}
