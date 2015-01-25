{
  allowUnfree = true;

  packageOverrides = super: let self = super.pkgs; in {
    inherit (self.stdenv) isDarwin isLinux;
    inherit (self.stdenv.lib) optionals;

    myUserPackages = self.buildEnv {
      name = "my-user-packages";
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
          vicare
        ];
    };

    vicare = self.callPackage ./pkgs/vicare { };
  };
}
