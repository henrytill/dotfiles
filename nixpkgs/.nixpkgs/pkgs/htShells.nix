{ stdenv, jdk7, jdk8, leiningen, maven, nodejs, sbt, haskellPackages }:

{
  jdk7-sbt-bnfc =
    let
      myJDK = jdk7;
      mySbt = sbt.override { jre = myJDK.jre; };
      bnfc  = haskellPackages.BNFC;
    in
    stdenv.mkDerivation {
      name = "shell-jdk7-sbt-bnfc";
      src = null;
      buildInputs = [ myJDK mySbt bnfc ];
      JAVA_HOME = "${myJDK}/lib/openjdk";
    };

  jdk7-sbt-maven-bnfc =
    let
      myJDK = jdk7;
      mySbt = sbt.override { jre = myJDK.jre; };
      bnfc  = haskellPackages.BNFC;
    in
    stdenv.mkDerivation {
      name = "shell-jdk7-sbt-maven-bnfc";
      src = null;
      buildInputs = [ myJDK mySbt maven bnfc ];
      JAVA_HOME = "${myJDK}/lib/openjdk";
    };

  jdk7-sbt-nodejs =
    let
      myJDK = jdk7;
      mySbt = sbt.override { jre = myJDK.jre; };
    in
    stdenv.mkDerivation {
      name = "shell-jdk7-sbt-nodejs";
      src = null;
      buildInputs = [ myJDK mySbt nodejs ];
      JAVA_HOME = "${myJDK}/lib/openjdk";
    };

  jdk8-sbt =
    let
      myJDK = jdk8;
      mySbt = sbt.override { jre = myJDK.jre; };
    in
    stdenv.mkDerivation {
      name = "shell-jdk8-sbt";
      src = null;
      buildInputs = [ myJDK mySbt ];
      JAVA_HOME = "${myJDK}/lib/openjdk";
    };

  jdk8-sbt-nodejs =
    let
      myJDK = jdk8;
      mySbt = sbt.override { jre = myJDK.jre; };
    in
    stdenv.mkDerivation {
      name = "shell-jdk8-sbt-nodejs";
      src = null;
      buildInputs = [ myJDK mySbt nodejs ];
      JAVA_HOME = "${myJDK}/lib/openjdk";
    };
}
