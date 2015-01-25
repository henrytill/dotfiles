{ stdenv, fetchurl, autoconf, automake, gmp
, libffi, libtool, readline, texinfo
}:

stdenv.mkDerivation rec {
  version = "0.3d7";
  name = "vicare-${version}";

  src = fetchurl {
    url = "https://github.com/marcomaggi/vicare/archive/${version}.tar.gz";
    sha256 = "16bry4awx701rgpdn5kz3l9i3illsrrp03fi5ckkl50wigx6x9nf";
  };

  buildInputs = [ autoconf automake gmp libffi libtool readline texinfo ];

  preConfigure = ''
    sh autogen.sh
    mkdir build
    cd build
  '';

  configureScript = "../configure";
  
  configureFlags = "--enable-maintainer-mode";

  meta = with stdenv.lib; {
    description = "A native compiler for Scheme compliant with R6RS";
    homepage = http://marcomaggi.github.io/vicare.html;
    license = licenses.gpl3;
    maintainers = with maintainers; [ henrytill ];
  };
}
