{ stdenv
, fetchurl
, aspell
, cacert
, cmake
, curl
, gnutls
, libgcrypt
, ncurses
, openssl
, pkgconfig
, zlib
}:

stdenv.mkDerivation rec {
  version = "1.1.1";
  name = "weechat-minimal-${version}";

  src = fetchurl {
    url = "http://weechat.org/files/src/weechat-${version}.tar.gz";
    sha256 = "0j8kc2zsv7ybgq6wi0r8siyd3adl3528gymgmidijd78smbpwbx3";
  };

  buildInputs =
    [ aspell
      cacert
      cmake
      curl
      gnutls
      libgcrypt
      ncurses
      openssl
      pkgconfig
      zlib
    ];

  cmakeFlags =
    [ "-DCA_FILE=${cacert}/etc/ca-bundle.crt"
      "-DENABLE_GUILE=OFF"
      "-DENABLE_LUA=OFF"
      "-DENABLE_PERL=OFF"
      "-DENABLE_PYTHON=OFF"
      "-DENABLE_RUBY=OFF"
      "-DENABLE_TCL=OFF"
    ];

  meta = with stdenv.lib; {
    description = "A fast, light and extensible chat client";
    homepage = http://www.weechat.org/;
    license = licenses.gpl3;
    maintainers = maintainers.henrytill;
    platforms = platforms.unix;
  };
}
