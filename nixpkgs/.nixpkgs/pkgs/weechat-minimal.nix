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
  version = "1.7";
  name = "weechat-minimal-${version}";

  src = fetchurl {
    url = "http://weechat.org/files/src/weechat-${version}.tar.gz";
    sha256 = "1ss29lvvrbb87dik2y26xqb4bnp59swcfran6wz5zg8w2i8alqfp";
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
