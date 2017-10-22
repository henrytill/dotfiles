{
  allowUnfree = true;

  packageOverrides = super: let self = super.pkgs; in {

    haskell =
      let
        lib = self.haskell.lib;
      in
      super.haskell // {
        packages = super.haskell.packages // {
          ghc7103_async_2_0_2 = super.haskell.packages.ghc7103.override {
            overrides = self: super: {
              async = super.callHackage "async" "2.0.2" {};
            };
          };
          ghc802_optparse-applicative_0_14_0_0 = super.haskell.packages.ghc802.override {
            overrides = self: super: {
              optparse-applicative = super.optparse-applicative_0_14_0_0;
            };
          };
          ghc802_syb_0_6 = super.haskell.packages.ghc802.override {
            overrides = self: super: {
              syb = super.callHackage "syb" "0.6" {};
            };
          };
        };
      };

    haskellPackages =
      let
        lib = self.haskell.lib;
        darwinStaticExe = p: if self.stdenv.isDarwin then lib.justStaticExecutables p else p;
        home = builtins.getEnv "HOME";
        distProcSimpPath = home + "/src/study-haskell/parconc/distributed-process-simplelocalnet";
      in
      super.haskellPackages.override {
        overrides = self: super: {
          cabal2nix                          = darwinStaticExe super.cabal2nix;
          darcs                              = darwinStaticExe super.darcs;
          distributed-process-simplelocalnet = super.callPackage distProcSimpPath {};
          network-transport                  = super.network-transport_0_5_2;
          network-transport-tcp              = super.network-transport-tcp_0_6_0;
          lhs2tex                            = darwinStaticExe super.lhs2tex;
          stack                              = darwinStaticExe super.stack;
          threadscope                        = darwinStaticExe super.threadscope;
        };
      };

    ht = {
      packages   = self.callPackage ./pkgs/ht/packages.nix {};
      scripts    = self.recurseIntoAttrs (self.callPackage ./pkgs/ht/scripts.nix {});
      shells     = self.recurseIntoAttrs (self.callPackage ./pkgs/ht/shells.nix  {});
      texliveEnv = self.texlive.combine {
        inherit (self.texlive)
        scheme-medium
        collection-fontsextra
        collection-fontsrecommended
        collection-latexextra
        enumitem
        fontaxes
        mweights
        svg
        titlesec;
      };
    };

    nix = super.nix.overrideAttrs (oldAttrs: {
      meta = (oldAttrs.meta // { outputsToInstall = [ "out" "man" ]; });
    });

    weechat = self.callPackage ./pkgs/weechat-minimal.nix {};
  };
}
