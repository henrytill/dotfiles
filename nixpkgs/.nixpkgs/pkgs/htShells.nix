{ stdenv, pkgs }:

rec {

  haskell =
    stdenv.mkDerivation {
      name = "shell-haskell";
      src = null;
      buildInputs = [
        (pkgs.haskellPackages.ghcWithPackages (hsPkgs: with hsPkgs; [
          mtl
        ]))
        pkgs.haskellPackages.doctest
      ];
    };

  jdk-sbt = { jdk }:
    let
      sbt  = pkgs.sbt.override { jre = jdk.jre; };
    in
    stdenv.mkDerivation {
      name = "shell-${jdk.name}-sbt";
      src = null;
      buildInputs = [ jdk sbt ];
      JAVA_HOME = "${jdk}";
    };

  jdk-sbt-nodejs = { jdk }:
    let
      sbt  = pkgs.sbt.override { jre = jdk.jre; };
    in
    stdenv.mkDerivation {
      name = "shell-${jdk.name}-sbt-nodejs";
      src = null;
      buildInputs = [ jdk sbt pkgs.nodejs ];
      JAVA_HOME = "${jdk}";
    };

  jdk-maven = { jdk }:
    let
      maven  = pkgs.maven.override { jdk = jdk; };
    in
    stdenv.mkDerivation {
      name = "shell-${jdk.name}-maven";
      src = null;
      buildInputs = [ jdk maven ];
      JAVA_HOME = "${jdk}";
    };

  jdk-lein = { jdk }:
    let
      leiningen  = pkgs.leiningen.override { jdk = jdk; };
    in
    stdenv.mkDerivation {
      name = "shell-${jdk.name}-leiningen";
      src = null;
      buildInputs = [ jdk leiningen ];
      JAVA_HOME = "${jdk}";
    };

  jdk7-sbt        = jdk-sbt        { jdk = pkgs.jdk7; };
  jdk8-sbt        = jdk-sbt        { jdk = pkgs.jdk8; };
  jdk7-sbt-nodejs = jdk-sbt-nodejs { jdk = pkgs.jdk7; };
  jdk8-sbt-nodejs = jdk-sbt-nodejs { jdk = pkgs.jdk8; };
  jdk7-maven      = jdk-maven      { jdk = pkgs.jdk7; };
  jdk8-maven      = jdk-maven      { jdk = pkgs.jdk8; };
  jdk7-lein       = jdk-lein       { jdk = pkgs.jdk7; };
  jdk8-lein       = jdk-lein       { jdk = pkgs.jdk8; };
}
