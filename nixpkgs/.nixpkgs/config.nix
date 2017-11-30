{
  allowUnfree = true;

  packageOverrides = super: let self = super.pkgs; in {

    haskell =
      let lib = self.haskell.lib;
      in super.haskell // {
        packages = super.haskell.packages // {
          ghc7103_async_2_0_2 =
            super.haskell.packages.ghc7103.override {
              overrides = self: super: {
                async = super.callHackage "async" "2.0.2" {};
              };
            };
          ghc802_Cabal_2_0_0_2 =
            let storeSrc = super.pkgs.fetchFromGitHub {
                             owner           = "fpco";
                             repo            = "store";
                             rev             = "c5b6e34ebe4f74a971a8d7428c4cd1fe9bfeecf1";
                             sha256          = "1nw33pdvwgbp1nhz474rcns38a0qvbhqlh5pwagz0jxhnvnsh6w1";
                             fetchSubmodules = true;
                           };
            in super.haskell.packages.ghc802.override {
              overrides = self: super: {
                Cabal            = super.Cabal_2_0_0_2;
                stackage-curator = super.stackage-curator_0_15_1_0;
                store            = super.callCabal2nix "store" storeSrc {};
              };
            };
          ghc802_optparse-applicative_0_14_0_0 =
            super.haskell.packages.ghc802.override {
              overrides = self: super: {
                optparse-applicative = super.optparse-applicative_0_14_0_0;
              };
            };
          ghc802_syb_0_6 =
            super.haskell.packages.ghc802.override {
              overrides = self: super: {
                syb = super.callHackage "syb" "0.6" {};
              };
            };
        };
      };

    haskellPackages =
      let darwinStaticExe = p: if self.stdenv.isDarwin then lib.justStaticExecutables p else p;
          home            = builtins.getEnv "HOME";
          lib             = self.haskell.lib;
          distProcSimp    = super.pkgs.fetchFromGitHub {
                              owner           = "haskell-distributed";
                              repo            = "distributed-process-simplelocalnet";
                              rev             = "3e1660fdbd82995ebb05f3c37991597728f622c6";
                              sha256          = "1yg72lksdqwiy80l71z0xrdzjgnmppa29lzfk16bss6rh51xj4jy";
                              fetchSubmodules = true;
                            };
          multiGHCTravis  = super.pkgs.fetchFromGitHub {
                              owner           = "hvr";
                              repo            = "multi-ghc-travis";
                              rev             = "a76b3e96a796936b750efbd555cce5714e752f97";
                              sha256          = "122bdaszr9nl1nilslc1kxb954v34b72xasqvsplkgby1hzlzgfi";
                              fetchSubmodules = true;
                            };
         mgt              = super.haskellPackages.callCabal2nix "multi-ghc-travis" multiGHCTravis {};
      in super.haskellPackages.override {
        overrides = self: super: {
          Agda                               = darwinStaticExe super.Agda;
          cabal2nix                          = darwinStaticExe super.cabal2nix;
          darcs                              = darwinStaticExe super.darcs;
          distributed-process-simplelocalnet = super.callCabal2nix "distributed-process-simplelocalnet" distProcSimp {};
          idris                              = darwinStaticExe super.idris;
          multi-ghc-travis                   = darwinStaticExe mgt;
          network-transport                  = super.network-transport_0_5_2;
          network-transport-tcp              = super.network-transport-tcp_0_6_0;
          lhs2tex                            = darwinStaticExe super.lhs2tex;
          stack                              = darwinStaticExe super.stack;
          stylish-haskell                    = darwinStaticExe super.stylish-haskell;
          threadscope                        = darwinStaticExe super.threadscope;
        };
      };

    ht = {
      packages   = super.callPackage ./pkgs/ht/packages.nix {};
      scripts    = super.recurseIntoAttrs (super.callPackage ./pkgs/ht/scripts.nix {});
      shells     = super.recurseIntoAttrs (super.callPackage ./pkgs/ht/shells.nix  {});
      texliveEnv = super.texlive.combine {
        inherit (super.texlive)
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
