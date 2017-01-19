# -*- sh-indent-for-case-label: 0; sh-indent-for-case-alt: +; -*-

case $(uname) in

"Darwin")
    if [[ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]]; then
        source $HOME/.nix-profile/etc/profile.d/nix.sh
        export NIX_PATH=nixpkgs=$HOME/src/nixpkgs
    fi

    if [[ -n $NIX_LINK && -d $NIX_LINK/share/man ]]; then
        export MANPATH=$NIX_LINK/share/man:$MANPATH
    fi

    if [[ -n $NIX_LINK && -f $NIX_LINK/etc/X11/fonts.conf ]]; then
        export FONTCONFIG_FILE=$NIX_LINK/etc/X11/fonts.conf
    fi

    if [[ -n $NIX_LINK && -d $NIX_LINK/lib/aspell ]]; then
        export ASPELL_CONF="dict-dir $NIX_LINK/lib/aspell"
    fi

    if [[ -e /usr/libexec/java_home ]]; then
        export JAVA_HOME=$(/usr/libexec/java_home)
    fi

    if [[ -d /opt/apache-maven-3.3.3 ]]; then
        export PATH=/opt/apache-maven-3.3.3/bin:$PATH
    fi

    if [[ -d /opt/gradle-2.4 ]]; then
        export GRADLE_HOME=/opt/gradle-2.4
        export PATH=$GRADLE_HOME/bin:$PATH
    fi

    if [[ -d /opt/protoc-3.0.2-osx-x86_64 ]]; then
        export PATH=/opt/protoc-3.0.2-osx-x86_64/bin:$PATH
    fi

    if [[ -d /Library/Frameworks/Mono.framework ]]; then
        export PATH=$PATH:/Library/Frameworks/Mono.framework/Versions/Current/bin
    fi

    if [[ -d $HOME/.gem/ruby/2.0.0/bin ]]; then
        export PATH=$HOME/.gem/ruby/2.0.0/bin:$PATH
    fi

    if [[ -d $HOME/Library/Python/2.7/bin ]]; then
        export PATH=$HOME/Library/Python/2.7/bin:$PATH
    fi

    if [[ -d /Volumes/vms/vagrant.d ]]; then
        export VAGRANT_HOME=/Volumes/vms/vagrant.d
    fi

    export LANG=en_US.UTF-8

    RUST_TOOLCHAIN="stable-x86_64-apple-darwin"
    ;;

"Linux")
    RUST_TOOLCHAIN="stable-x86_64-unknown-linux-gnu"
    ;;

esac

if [[ -d $HOME/.multirust/toolchains/$RUST_TOOLCHAIN ]]; then
    export RUST_SRC_PATH=$HOME/.multirust/toolchains/$RUST_TOOLCHAIN/lib/rustlib/src/rust/src
fi

if [[ -n $(command -v opam) && -d $HOME/.opam ]]; then
    . $HOME/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
fi

if [[ -d $HOME/.cargo/bin ]]; then
    export PATH=$HOME/.cargo/bin:$PATH
fi

if [[ -d $HOME/.conscript ]]; then
    export CONSCRIPT_HOME="$HOME/.conscript"
    export CONSCRIPT_OPTS="-XX:MaxPermSize=512M -Dfile.encoding=UTF-8"
    export PATH=$CONSCRIPT_HOME/bin:$PATH
fi

if [[ -d $HOME/bin ]]; then
    export PATH=$HOME/bin:$PATH
fi

if [[ -n $(command -v lein) ]]; then
    export LEIN_FAST_TRAMPOLINE=y
fi

export EDITOR="vim"

export _JAVA_AWT_WM_NONREPARENTING=1

# Only unique entries please.
typeset -U path
