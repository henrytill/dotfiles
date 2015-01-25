# .bashrc

if [ "$(uname -s)" = "Darwin" ]; then

    case "$TERM" in
        xterm|xterm-256color|screen-256color|eterm-color|rxvt*)
            color_prompt=yes;;
        dumb)
            color_prompt=no;;
    esac

    PROMPT_COLOR="1;32m"

    if [ -n "$NIX_MYENV_NAME" -a "$color_prompt" = yes ]; then
        PS1='\n\[\033[$PROMPT_COLOR\]$NIX_MYENV_NAME:[\u@\h:\w]\$ \[\033[0m\]'
    elif [ -n "$NIX_MYENV_NAME" ]; then
        PS1='\n$NIX_MYENV_NAME:[\u@\h:\w]\$ '
    elif [ "$color_prompt" = yes ]; then
        PS1='\n\[\033[$PROMPT_COLOR\][\u@\h:\w]\$ \[\033[0m\]'
    else
        PS1='\n[\u@\h:\w]\$ '
    fi
    unset color_prompt

    if [ -f "$HOME/.bash_aliases" ]; then
        . "$HOME/.bash_aliases"
    fi

    if [ "$(type -P ls)" = "$HOME/.nix-profile/bin/ls" ]; then
        alias ls="ls --color=tty"
    else
        alias ls="ls -G"
    fi
    
    # OPAM configuration
    if [ -d "$HOME/.opam" -a -n "$(type -P opam)" ]; then
        . "$HOME/.opam/opam-init/init.sh" > /dev/null 2> /dev/null || true
    fi

    # Display Nix profile
    if [ -e "$HOME/.nix-profile" -a -n "$(type -P nix-env)" ]; then
        p() {
            clear
            echo 'Current Profile: ' && readlink "$HOME/.nix-profile"
            echo && echo 'Installed:' && nix-env -q
        }
    fi

    export GPG_TTY="$(tty)"

fi
