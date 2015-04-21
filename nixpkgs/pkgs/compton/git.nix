{ stdenv
, fetchFromGitHub
, asciidoc
, dbus
, docbook_xml_dtd_45
, docbook_xml_xslt
, libX11
, libXcomposite
, libXdamage
, libXext
, libXfixes
, libXinerama
, libXrandr
, libXrender
, libconfig
, libdrm
, libxml2
, libxslt
, mesa
, pcre
, pkgconfig
, xproto
}:

stdenv.mkDerivation rec {

  name = "compton-git";

  src = fetchFromGitHub {
    owner  = "chjj";
    repo   = "compton";
    rev    = "23d1dd1c0e23f1c0b79cd329c088bb7483357fd8";
    sha256 = "0s7v8zv8n2rnifyyqg9mgn0yb0xrzaqfzghzppsxy3lphr908dm9";
  };

  buildInputs =
    [ asciidoc
      dbus
      docbook_xml_dtd_45
      docbook_xml_xslt
      libX11
      libXcomposite
      libXdamage
      libXext
      libXfixes
      libXinerama
      libXrandr
      libXrender
      libconfig
      libdrm
      libxml2
      libxslt
      mesa
      pcre
      pkgconfig
      xproto
    ];

  installFlags = "PREFIX=$(out)";

  meta = with stdenv.lib; {
    homepage = https://github.com/chjj/compton/;
    description = "A fork of XCompMgr, a sample compositing manager for X servers";
    longDescription = ''
      A fork of XCompMgr, which is a  sample compositing manager for X servers
      supporting the XFIXES, DAMAGE, RENDER, and COMPOSITE extensions.  It enables
      basic eye-candy effects. This fork adds additional features, such as additional
      effects, and a fork at a well-defined and proper place.
    '';
    license = licenses.mit;
    platforms = platforms.linux;
  };
}
