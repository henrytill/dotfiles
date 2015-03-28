{
  allowUnfree = true;

  packageOverrides = super: let self = super.pkgs; in {

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
          [ emacs
            haskellEnv
            (gitAndTools.gitFull.override { guiSupport = false; })
            gnumake
            gnupg1compat
            mr
            offlineimap
            rsync
            stow
            tmux
            tree
            weechat-minimal
            wget
            xz
            youtube-dl
            zsh
          ] ++ optionals isLinux
          [ leiningen
          ] ++ optionals isDarwin
          [ xquartz
            xterm
          ];
      };

    emacs = if self.stdenv.isDarwin
      then super.emacs24Macport
      else super.emacs;

    pinentry = super.pinentry.override { gtk2 = null; };

    vicare = self.callPackage ./pkgs/vicare { };

    weechat-minimal = self.callPackage ./pkgs/weechat/weechat-minimal.nix { };

    youtube-dl = self.stdenv.lib.overrideDerivation super.youtube-dl (attrs: {
      ffmpeg = null;
      postInstall = "";
    });

  };
}
