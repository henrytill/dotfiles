{
  allowUnfree = true;

  firefox = {
    enableGoogleTalkPlugin = true;
  };

  packageOverrides = super: let self = super.pkgs; in {
    graphviz =
      if super.stdenv.isDarwin
        then super.graphviz.override { xlibs = null; }
        else super.graphviz;
  };
}
