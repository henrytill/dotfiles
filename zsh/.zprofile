# .zprofile

if [[ $(uname) == Darwin ]]; then

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

    if [[ -d /opt/mongodb ]]; then
        export PATH=/opt/mongodb/bin:$PATH
    fi

    if [[ -d /opt/rabbitmq_server-3.0.2 ]]; then
        export PATH=/opt/rabbitmq_server-3.0.2/sbin:$PATH
    fi

    if [[ -d /opt/protoc-3.0.2-osx-x86_64 ]]; then
        export PATH=/opt/protoc-3.0.2-osx-x86_64/bin:$PATH
    fi

    if [[ -d /Library/Frameworks/Mono.framework ]]; then
        export PATH=/Library/Frameworks/Mono.framework/Versions/Current/bin:$PATH
    fi

    if [[ -d $HOME/./multirust/toolchains/stable-x86_64-apple-darwin ]]; then
        export RUST_SRC_PATH=$HOME/.multirust/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src
    fi

    if [[ -d $HOME/.gem/ruby/2.0.0/bin ]]; then
        export PATH=$HOME/.gem/ruby/2.0.0/bin:$PATH
    fi

    if [[ -d /opt/omnicore-0.0.11.1-rel ]]; then
        export PATH=/opt/omnicore-0.0.11.1-rel/bin:$PATH
    fi

    if [[ -d /Volumes/vms/vagrant.d ]]; then
        export VAGRANT_HOME=/Volumes/vms/vagrant.d
    fi

    export LANG=en_US.UTF-8
fi

if [[ ! -d /etc/nixos ]]; then
    if [[ -n $(command -v opam) && -d $HOME/.opam ]]; then
        . $HOME/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
    fi

    if [[ -d $HOME/.cargo/bin ]]; then
        export PATH=$HOME/.cargo/bin:$PATH
    fi

    if [[ -d $HOME/bin ]]; then
        export PATH=$HOME/bin:$PATH
    fi
fi

if [[ -n $(command -v lein) ]]; then
    export LEIN_FAST_TRAMPOLINE=y
fi

if [[ -d $HOME/node_modules/.bin ]]; then
    export PATH=$HOME/node_modules/.bin:$PATH
fi

export EDITOR="vim"
