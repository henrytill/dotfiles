{ stdenv, fetchurl, bash, jdk, writeScript }:

stdenv.mkDerivation rec {
  name = "giter8-${version}";
  version = "0.6.8";

  sbt-launch = fetchurl {
    url = https://dl.bintray.com/typesafe/ivy-releases/org.scala-sbt/sbt-launch/0.13.9/sbt-launch.jar;
    sha256 = "04k411gcrq35ayd2xj79bcshczslyqkicwvhkf07hkyr4j3blxda";
  };

  launchconfig = fetchurl {
    url = https://raw.githubusercontent.com/n8han/giter8/0.6.8/src/main/conscript/g8/launchconfig;
    sha256 = "1q2z6239w9xgpkqa3g7c5i19y2g2s76wbbmz9iqa3lj70wn07c88";
  };

  phases = [ "installPhase" ];

  installPhase = ''
    mkdir -p $out/bin

    cat > $out/bin/g8 << EOF
    #! ${bash}/bin/bash
    exec ${jdk}/bin/java -Xmx512M -jar ${sbt-launch} @${launchconfig} "\$@"
    EOF

    chmod a+x $out/bin/g8
  '';

  meta = with stdenv.lib; {
    homepage = https://github.com/n8han/giter8;
    license = licenses.lgpl3;
    description = "A command line tool to apply templates defined on github";
    maintainers = [ maintainers.henrytill ];
  };
}
