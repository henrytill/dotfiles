# .zprofile

export_dir () {
    if [[ -d $2 ]]
    then
        export $1="$2"
    fi
}

add_dir_to_path_front () {
    if [[ -d $1 ]]
    then
        export PATH="$1:$PATH"
    fi
}

add_dir_to_path_back () {
    if [[ -d $1 ]]
    then
        export PATH="$PATH:$1"
    fi
}

is_nix_machine () {
    $HOST == "thaumas" || $HOST == "proteus"
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

        if [[ -d "/usr/local/lib/pkgconfig" ]]
        then
            export PKG_CONFIG_PATH="$PKG_CONFIG_PATH:/usr/local/lib/pkgconfig"
        fi

        # Plan 9 from User Space
        export_dir PLAN9 "/opt/plan9port"
        add_dir_to_path_back "$PLAN9/bin"

        add_dir_to_path_front "$HOME/Library/Python/2.7/bin"
        add_dir_to_path_front "/opt/apache-maven-3.5.4/bin"
        add_dir_to_path_front "/opt/protoc-3.6.0-osx-x86_64/bin"
        ;;

    "Linux")
        if [[ is_nix_machine && -e "$HOME/.nix-profile/etc/profile.d/nix.sh" ]]
        then
            source "$HOME/.nix-profile/etc/profile.d/nix.sh"
        fi

        add_dir_to_path_front "$HOME/.local/bin"
        ;;
esac

if [[ -n "$(command -v vim)" ]]
then
    export EDITOR="vim"
fi

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
export_dir PATSHOME "/opt/ats"
export_dir PATSCONTRIB "$HOME/src/other/ATS-Postiats-contrib"

if [[ -n $PATSHOME ]]
then
    add_dir_to_path_front "$PATSHOME/bin"
fi

# Haskell
add_dir_to_path_front "/opt/ghc/bin"
add_dir_to_path_front "/opt/cabal/bin"
add_dir_to_path_front "$HOME/.cabal/bin"

# Rust
add_dir_to_path_front "$HOME/.cargo/bin"

# Hashicorp
add_dir_to_path_front "/opt/packer/bin"
add_dir_to_path_front "/opt/terraform/bin"

# scripts
add_dir_to_path_front "$HOME/bin"
