{
  allowUnfree = true;

  firefox = {
    enableAdobeFlash = true;
    enableGoogleTalkPlugin = true;
  };

  packageOverrides = super: let self = super.pkgs; in {};
}
