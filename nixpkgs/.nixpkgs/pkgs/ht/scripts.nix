{ stdenv, pkgs }:

{
  # https://github.com/NixOS/nixpkgs/issues/16263#issue-160547656
  nix-build-remote =
    let
      remoteSystems = pkgs.writeText "remote-systems.conf" ''
        nix@builder x86_64-linux /Users/ht/.ssh/builder-id_rsa 1 1 kvm
      '';
    in
    pkgs.writeScriptBin "nix-build-remote" ''
      #!${pkgs.stdenv.shell}
      mkdir -p /tmp/build-remote-load
      chmod a+rwX /tmp/build-remote-load

      export NIX_BUILD_HOOK="${pkgs.nix.out}/libexec/nix/build-remote.pl"
      export NIX_REMOTE_SYSTEMS="${remoteSystems}"
      export NIX_CURRENT_LOAD="/tmp/build-remote-load"
      exec "${pkgs.nix.out}/bin/nix-build" "$@"
    '';
}
