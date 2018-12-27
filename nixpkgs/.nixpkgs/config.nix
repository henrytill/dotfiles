{
  allowUnfree = true;

  packageOverrides = super: let self = super.pkgs; in {

    haskell =
      let
        lib = self.haskell.lib;
        home = builtins.getEnv "HOME";
      in
        super.haskell // {
          packages = super.haskell.packages // {
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
                    distributed-process-simplelocalnet = self.callCabal2nix "distributed-process-simplelocalnet" dpsSrc {};
                  };
                };
          };
        };

    haskellPackages =
      let
        callCabal2nix   = self.haskellPackages.callCabal2nix;
        darwinStaticExe = p: if self.stdenv.isDarwin then lib.justStaticExecutables p else p;
        home            = builtins.getEnv "HOME";
        lib             = self.haskell.lib;
        haskellCISrc    = super.pkgs.fetchFromGitHub {
                            owner           = "haskell-CI";
                            repo            = "haskell-ci";
                            rev             = "b592d290cff68c7abcbd7f99f41aac998e7b7916";
                            sha256          = "0jwaifp9wx6lb27qj191hjm125kq3vndh5lf7ibd86x19h5zmy4c";
                            fetchSubmodules = true;
                          };
        haskellCI       = callCabal2nix "haskell-ci" haskellCISrc {};
      in
        super.haskellPackages.override {
          overrides = self: super: {
            Agda            = darwinStaticExe super.Agda;
            brittany        = darwinStaticExe super.brittany;
            cabal2nix       = darwinStaticExe super.cabal2nix;
            darcs           = darwinStaticExe super.darcs;
            ghcid           = darwinStaticExe super.ghcid;
            hasktags        = darwinStaticExe super.hasktags;
            idris           = darwinStaticExe super.idris;
            lhs2tex         = darwinStaticExe super.lhs2tex;
            stack           = darwinStaticExe super.stack;
            stylish-haskell = darwinStaticExe super.stylish-haskell;
            threadscope     = darwinStaticExe super.threadscope;
          };
        };

    ht = {
      packages   = self.recurseIntoAttrs (self.callPackage ./pkgs/ht/packages.nix {});
      scripts    = self.recurseIntoAttrs (self.callPackage ./pkgs/ht/scripts.nix  {});
      shells     = self.recurseIntoAttrs (self.callPackage ./pkgs/ht/shells.nix   {});
      texliveEnv = self.texlive.combine {
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
