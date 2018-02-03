{
  allowUnfree = true;

  packageOverrides = super: let self = super.pkgs; in {

    haskell =
      let
        lib = self.haskell.lib;
      in
        super.haskell // {
          packages = super.haskell.packages // {
            ghc7103_async_2_0_2 =
              super.haskell.packages.ghc7103.override {
                overrides = self: super: {
                  async = super.callHackage "async" "2.0.2" {};
                };
              };
            ghc802_parconc =
              let
                dpsSrc = super.pkgs.fetchFromGitHub {
                           owner           = "haskell-distributed";
                           repo            = "distributed-process-simplelocalnet";
                           rev             = "3e1660fdbd82995ebb05f3c37991597728f622c6";
                           sha256          = "1yg72lksdqwiy80l71z0xrdzjgnmppa29lzfk16bss6rh51xj4jy";
                           fetchSubmodules = true;
                         };
              in
                super.haskell.packages.ghc802.override {
                  overrides = self: super: {
                    network-transport                  = super.network-transport_0_5_2;
                    network-transport-tcp              = super.network-transport-tcp_0_6_0;
                    distributed-process-simplelocalnet = super.callCabal2nix "distributed-process-simplelocalnet" dpsSrc {};
                  };
                };
          };
        };

    haskellPackages =
      let
        darwinStaticExe = p: if self.stdenv.isDarwin then lib.justStaticExecutables p else p;
        home            = builtins.getEnv "HOME";
        lib             = self.haskell.lib;
        bnfcPath        = home + "/src/other/bnfc/source/BNFC.nix";
      in
        super.haskellPackages.override {
          overrides = self: super: {
            Agda            = darwinStaticExe super.Agda;
            BNFC            = darwinStaticExe (super.callPackage bnfcPath {});
            brittany        = darwinStaticExe super.brittany;
            cabal2nix       = darwinStaticExe super.cabal2nix;
            darcs           = darwinStaticExe super.darcs;
            idris           = darwinStaticExe super.idris;
            lhs2tex         = darwinStaticExe super.lhs2tex;
            stack           = darwinStaticExe super.stack;
            stylish-haskell = darwinStaticExe super.stylish-haskell;
            threadscope     = darwinStaticExe super.threadscope;
          };
        };

    ht = {
      packages   = super.recurseIntoAttrs (super.callPackage ./pkgs/ht/packages.nix {});
      scripts    = super.recurseIntoAttrs (super.callPackage ./pkgs/ht/scripts.nix  {});
      shells     = super.recurseIntoAttrs (super.callPackage ./pkgs/ht/shells.nix   {});
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
