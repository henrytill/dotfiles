{
  allowUnfree = true;

  packageOverrides = super: let self = super.pkgs; in {

    emacs = if self.stdenv.isDarwin
      then super.emacs24Macport
      else super.emacs;

    baseEnv =
      let
        inherit (self.stdenv) isDarwin isLinux;
        inherit (self.stdenv.lib) optionals;
        isNixOS = (isLinux && builtins.pathExists /etc/nixos);

        haskellEnv = self.haskellngPackages.ghcWithPackages (p: with p; [
          cabal-install
          cabal2nix
          hlint
        ]);

      in self.buildEnv {
        name = "my-base-environment";
        paths = with self;
          [ guile
            nix-repl
            pandoc
          ] ++ optionals (!isNixOS)
          [ haskellEnv
            emacs
            (gitAndTools.gitFull.override { guiSupport = false; })
            gnumake
            gnupg1compat
            mr
            offlineimap
            (pinentry.override { useGtk = false; })
            rsync
            stow
            tmux
            tree
            wget
            xz
            youtube-dl
          ] ++ optionals isLinux
          [ leiningen ];
      };

    vicare = self.callPackage ./pkgs/vicare { };

  };
}
