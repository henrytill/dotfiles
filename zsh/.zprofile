# .zprofile

export_dir ()
{
    if [[ -d $2 ]]
    then
        export $1="$2"
    fi
}

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

        export_dir VAGRANT_HOME "/Volumes/vms/vagrant.d"

        add_dir_to_path_back "/Library/Frameworks/Mono.framework/Versions/Current/bin"
        add_dir_to_path_front "/opt/protoc-3.4.0-osx-x86_64/bin"
        add_dir_to_path_front "/opt/apache-maven-3.5.0/bin"
        add_dir_to_path_front "$HOME/.gem/ruby/2.0.0/bin"
        add_dir_to_path_front "$HOME/Library/Python/2.7/bin"
        add_dir_to_path_front "/opt/ghc-8.0.2/bin"

        export LANG="en_US.UTF-8"

        export EDITOR="emacsclient -t --alternate-editor=vim"

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

        export EDITOR="emacsclient -t --alternate-editor="

        RUST_TOOLCHAIN="stable-x86_64-unknown-linux-gnu"
        ;;
esac

if [[ -n "$(command -v lein)" ]]
then
    export LEIN_FAST_TRAMPOLINE=y
fi

# Rust sources
export_dir RUST_SRC_PATH "$HOME/.multirust/toolchains/$RUST_TOOLCHAIN/lib/rustlib/src/rust/src"

# Conscript
export_dir CONSCRIPT_HOME "$HOME/.conscript"

if [[ -n $CONSCRIPT_HOME ]]
then
    add_dir_to_path_front "$CONSCRIPT_HOME/bin"
    export CONSCRIPT_OPTS="-XX:MaxPermSize=512M -Dfile.encoding=UTF-8"
fi

# Go
export_dir GOROOT "/opt/go"
export_dir GOPATH "$HOME/opt/go"

if [[ -n $GOROOT ]]
then
    add_dir_to_path_front "$GOROOT/bin"
fi

if [[ -n $GOPATH ]]
then
    add_dir_to_path_back "$GOPATH/bin"
fi

# ATS2
export_dir PATSHOME "/opt/ATS2-Postiats-0.3.6"
export_dir PATSCONTRIB "$HOME/src/other/ATS-Postiats-contrib"

if [[ -n $PATSHOME ]]
then
    add_dir_to_path_front "$PATSHOME/bin"
fi

# Yarn
add_dir_to_path_front "/opt/yarn/bin"

# SML
add_dir_to_path_front "/usr/local/share/smlnj/bin"

# Cargo
add_dir_to_path_front "$HOME/.cargo/bin"

# scripts
add_dir_to_path_front "$HOME/bin"
