# .zprofile

add_dir_to_path_front ()
{
    if [[ -d $1 ]]
    then
        export PATH="$1:$PATH"
    fi
}

add_dir_to_path_back ()
{
    if [[ -d $1 ]]
    then
        export PATH="$PATH:$1"
    fi
}

case $(uname -s) in
    "Darwin")
        if [[ -e "$HOME/.nix-profile/etc/profile.d/nix.sh" ]]
        then
            source "$HOME/.nix-profile/etc/profile.d/nix.sh"

            local nixpkgs="$HOME/src/nixpkgs"

            if [[ -d $nixpkgs && ! -h "$HOME/.nix-defexpr/channels" ]]
            then
                export NIX_PATH="nixpkgs=$nixpkgs"
            fi
        fi

        if [[ -n $NIX_LINK && -d "$NIX_LINK/share/man" ]]
        then
            export MANPATH="$NIX_LINK/share/man:$MANPATH"
        fi

        if [[ -n $NIX_LINK && -f "$NIX_LINK/etc/X11/fonts.conf" ]]
        then
            export FONTCONFIG_FILE="$NIX_LINK/etc/X11/fonts.conf"
        fi

        if [[ -n $NIX_LINK && -d "$NIX_LINK/lib/aspell" ]]
        then
            export ASPELL_CONF="dict-dir $NIX_LINK/lib/aspell"
        fi

        if [[ -e "/usr/libexec/java_home" ]]
        then
            export JAVA_HOME="$(/usr/libexec/java_home)"
        fi

        if [[ -d "/Volumes/vms/vagrant.d" ]]
        then
            export VAGRANT_HOME="/Volumes/vms/vagrant.d"
        fi

        add_dir_to_path_back "/Library/Frameworks/Mono.framework/Versions/Current/bin"
        add_dir_to_path_front "/opt/protoc-3.0.2-osx-x86_64/bin"
        add_dir_to_path_front "/opt/apache-maven-3.5.0/bin"
        add_dir_to_path_front "$HOME/.gem/ruby/2.0.0/bin"
        add_dir_to_path_front "$HOME/Library/Python/2.7/bin"

        export LANG="en_US.UTF-8"

        RUST_TOOLCHAIN="stable-x86_64-apple-darwin"
        ;;

    "Linux")
        if [[ $HOST == "thaumas" && -e "$HOME/.nix-profile/etc/profile.d/nix.sh" ]]
        then
            source "$HOME/.nix-profile/etc/profile.d/nix.sh"
        fi

        if [[ -d "/opt/mira-2042-i686-Linux" ]]
        then
            export MIRALIB="/opt/mira-2042-i686-Linux/lib/miralib"
            add_dir_to_path_front "/opt/mira-2042-i686-Linux/bin"
        fi

        RUST_TOOLCHAIN="stable-x86_64-unknown-linux-gnu"
        ;;
esac

if [[ -n "$(command -v vim)" ]]
then
    export EDITOR="vim"
fi

if [[ -n "$(command -v lein)" ]]
then
    export LEIN_FAST_TRAMPOLINE=y
fi

if [[ -d "$HOME/.multirust/toolchains/$RUST_TOOLCHAIN" ]]
then
    export RUST_SRC_PATH="$HOME/.multirust/toolchains/$RUST_TOOLCHAIN/lib/rustlib/src/rust/src"
fi

if [[ -d "$HOME/.conscript" ]]
then
    export CONSCRIPT_HOME="$HOME/.conscript"
    export CONSCRIPT_OPTS="-XX:MaxPermSize=512M -Dfile.encoding=UTF-8"
    add_dir_to_path_front "$CONSCRIPT_HOME/bin"
fi

if [[ -d "/opt/go" ]]
then
    export GOROOT="/opt/go"
    add_dir_to_path_front "$GOROOT/bin"
fi

if [[ -d "$HOME/opt/go" ]]
then
    export GOPATH="$HOME/opt/go"
    add_dir_to_path_back "$GOPATH/bin"
fi

# ATS2

PATSHOME="/opt/ATS2-Postiats-0.3.6"
PATSCONTRIB="$HOME/src/other/ATS-Postiats-contrib"

if [[ -d $PATSHOME ]]
then
    export PATSHOME
    add_dir_to_path_front "$PATSHOME/bin"

    if [[ -d $PATSCONTRIB ]]
    then
        export PATSCONTRIB
    fi
fi

add_dir_to_path_front "/opt/yarn/bin"
add_dir_to_path_front "/usr/local/share/smlnj/bin"
add_dir_to_path_front "$HOME/.cargo/bin"
add_dir_to_path_front "$HOME/bin"
