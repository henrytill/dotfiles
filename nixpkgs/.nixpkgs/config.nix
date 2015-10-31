{
  allowUnfree = true;

  firefox = {
    enableGoogleTalkPlugin = true;
  };

  packageOverrides = super: let self = super.pkgs; in {
    racket-minimal = super.callPackage ./pkgs/racket/minimal.nix {};
  };
}
