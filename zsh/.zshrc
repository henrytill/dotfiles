# .zshrc

setopt autocd
setopt beep
setopt extendedglob
setopt histfcntllock
setopt histignoredups
setopt sharehistory

export HISTFILE=~/.zsh_histfile
export HISTSIZE=100000
export SAVEHIST=100000

bindkey -e

# check if in docker container
if [ -f /.dockerenv ]
then
    IN_DOCKER_CONTAINER=1
fi

# prompt
create_prompt ()
{
    local inDocker=${IN_DOCKER_CONTAINER/1/"[docker]"}
    local inNixShell=${IN_NIX_SHELL/1/"[$name]"}

    if [[ $TERM == dumb ]]
    then
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

create_prompt

# completion
zstyle :compinstall filename '$HOME/.zshrc'
autoload -Uz compinit
compinit

autoload -Uz bashcompinit
bashcompinit

zstyle ':completion:*' menu select

# window titles
if [[ $TERM == rxvt* ]]
then
    function set-title() {
        echo -en "\e]2;$USER@$HOST: $2\a"
    }
    autoload -Uz add-zsh-hook
    add-zsh-hook preexec set-title
fi

# Darwin-specific config
if [[ $(uname) == Darwin ]]
then
    alias ls="ls -G"

    if [[  -d /Applications/Emacs.app/ ]]
    then
        alias Emacs.app="open -n -a /Applications/Emacs.app"
    fi
fi

# Linux-specific config
if [[ $(uname) == Linux ]]
then
    alias ls="ls --color"
fi

# aliases
alias e="$EDITOR"
alias lf="ls -aF"
alias ll="ls -la"
alias llt="ls -lat"
alias lt="ls -lt"
alias u="cd .. && l"

if [[ -n $(command -v view) ]]
then
    alias v="view"
fi

if [[ $TERM == dumb ]]
then
    alias less="cat"
    alias more="cat"
    alias l="ls -lh"
    alias la="ls -lah"
    export PAGER="cat"
else
    alias l="clear && ls -lh"
    alias la="clear && ls -lah"
fi

if [[ -n $(command -v htop) ]]
then
    alias htop="TERM=xterm htop"
fi

if [[ -n $(command -v nix-shell) ]]
then
    alias nix-zshell="nix-shell --command zsh"

    nix-default-shell ()
    {
        nix-shell -E "(import <nixpkgs> {}).callPackage ./default.nix {}" $*
    }
fi

if [[ -n $(command -v npm) ]]
then
    npm-exec ()
    {
        PATH=$(npm bin):$PATH $*
    }
fi

if [[ -n $(command -v opam) && -d $HOME/.opam ]]
then
    . $HOME/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
fi

if [[ -n $(command -v hecate) ]]
then
    eval "$(hecate --bash-completion-script hecate)"
fi
