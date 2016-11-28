# .zshrc

setopt autocd
setopt beep
setopt extendedglob
setopt histfcntllock
setopt histignoredups
setopt sharehistory

export HISTFILE=~/.histfile
export HISTSIZE=100000
export SAVEHIST=100000

bindkey -e

# check if in docker container
if [ -f /.dockerenv ]; then
    IN_DOCKER_CONTAINER=1
fi

# prompt
() {
    local inDocker=${IN_DOCKER_CONTAINER/1/"[docker]"}
    local inNixShell=${IN_NIX_SHELL/1/"[$name]"}

    if [[ $TERM == dumb ]]; then
        unsetopt zle

        local retStatus='[%?]'

        PROMPT=$'\n'$retStatus$inDocker$inNixShell'> '
        PROMPT2=$inDocker$nixshellMode'> '
        RPROMPT=''
    else
        local firstLine='%F{6}%B%n@%m:%~%f%b'
        local retStatus='%(?.[%?].%F{1}[%?]%f)'

        PROMPT=$'\n'$firstLine$'\n'$retStatus$inDocker$inNixShell'> '
        PROMPT2=$inDocker$nixshellMode'> '
        RPROMPT=''
    fi
}

# completion
zstyle :compinstall filename '$HOME/.zshrc'
autoload -Uz compinit
compinit

zstyle ':completion:*' menu select

# window titles
if [[ $TERM == rxvt* ]]; then
    function set-title() {
        echo -en "\e]2;$USER@$HOST: $2\a"
    }
    autoload -Uz add-zsh-hook
    add-zsh-hook preexec set-title
fi

# Darwin-specific config
if [[ $(uname) == Darwin ]]; then
    alias ls="ls -G"

    if [[  -d /Applications/Emacs.app/ ]]; then
        alias Emacs.app="open -n -a /Applications/Emacs.app"
    fi
fi

# Linux-specific config
if [[ $(uname) == Linux ]]; then
    alias ls="ls --color"
fi

# aliases
alias e="$EDITOR"
alias v="vim"
alias lf="ls -aF"
alias ll="ls -la"
alias llt="ls -lat"
alias lt="ls -lt"
alias u="cd .. && l"

if [[ $TERM == dumb ]]; then
    alias less="cat"
    alias more="cat"
    alias l="ls -lh"
    alias la="ls -lah"
    export PAGER="cat"
else
    alias l="clear && ls -lh"
    alias la="clear && ls -lah"
fi

if [[ -n $(command -v gpg2) ]]; then
    alias gpg="gpg2"
fi

if [[ -n $(command -v htop) ]]; then
    alias htop="TERM=xterm htop"
fi

if [[ -n $(command -v nix-shell) ]]; then
    alias nix-zshell="nix-shell --command zsh"

    # https://github.com/haskell/cabal/issues/3651#issuecomment-236697644
    nix-cabal () {
        CABAL=$(whence -p cabal)  # always use same cabal as current shell
        if [[ -a "$PWD/.shell.drv" ]]; then
            nix-shell --add-root "$PWD/.result" \
                      --indirect \
                      "$PWD/.shell.drv" \
                      --run "$CABAL $*"
        elif [[ -a "$PWD/shell.nix" ]]; then
            nix-instantiate --add-root "$PWD/.shell.drv" \
                            --indirect \
                            "$PWD/shell.nix" \
                            -A env
            nix-shell --add-root "$PWD/.result" \
                      --indirect \
                      "$PWD/.shell.drv" \
                      --run "$CABAL $*" || rm "$PWD/.shell.drv"
        else
            nix-shell --add-root "$PWD/.result" --indirect --run "$CABAL $*"
        fi
    }
fi

if [[ -n $(command -v npm) ]]; then
    npm-exec () { PATH=$(npm bin):$PATH $* }
fi
