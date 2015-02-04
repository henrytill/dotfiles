{
  allowUnfree = true;

  packageOverrides = super:
    let
      self = super.pkgs;
      inherit (self.stdenv) isDarwin isLinux;
      inherit (self.stdenv.lib) optionals;
    in
    {
      baseEnv = self.buildEnv {
        name = "my-base-environment";
        paths = with self;
          # Common Packages
          [ guile ]
          # Darwin-specific Packages
          ++ optionals isDarwin
          [ coreutils
            emacs24-nox
            (gitAndTools.gitFull.override { guiSupport = false; })
            gnumake
            gnupg1compat
            mosh
            mr
            offlineimap
            (pinentry.override { useGtk = false; })
            stow
            rsync
            tmux
            tree
            wget
            xz
            youtube-dl
          ]
          # Linux-specific Packages
          ++ optionals isLinux
          [ leiningen
            pandoc
          ];
      };

      vicare = self.callPackage ./pkgs/vicare { };
    };
}
