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
    myEclipse =
      with super.eclipses;
      eclipseWithPlugins
        { eclipse = eclipse_sdk_45;
          plugins = [ plugins.emacsplus ];
        };
    racket-minimal = super.callPackage ./pkgs/racket/minimal.nix {};
  };
}
