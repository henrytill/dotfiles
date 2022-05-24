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
        ;;

    "Linux")
        if [[ is_nix_machine && -e "$HOME/.nix-profile/etc/profile.d/nix.sh" ]]
        then
            source "$HOME/.nix-profile/etc/profile.d/nix.sh"
        fi

        # Haskell Binaries
        add_dir_to_path_front "$HOME/.cabal/bin"

        # Local Binaries
        add_dir_to_path_front "$HOME/.local/bin"
        ;;
esac

if [[ -n "$(command -v emacsclient)" ]]
then
    export EDITOR="emacsclient -t -a="
elif [[ -n "$(command -v vim)" ]]
then
    export EDITOR="vim"
fi

# Plan 9 from User Space
export_dir PLAN9 "/opt/plan9port"
add_dir_to_path_back "$PLAN9/bin"

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

# SBCL
export_dir SBCL_HOME "/opt/sbcl-2.1.10-x86-64-linux/lib/sbcl"

if [[ -n $SBCL_HOME ]]
then
    add_dir_to_path_front "/opt/sbcl-2.1.10-x86-64-linux/bin"
fi

# MLton
add_dir_to_path_front "/opt/mlton-20210117/bin"

# Rust
add_dir_to_path_front "$HOME/.cargo/bin"

# jetbrains toolbox
add_dir_to_path_front "$HOME/.local/share/JetBrains/Toolbox/bin"

# scripts
add_dir_to_path_front "$HOME/bin"
