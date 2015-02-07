# .bashrc

if [ "$(uname -s)" = "Darwin" ]; then

    case "$TERM" in
        xterm|xterm-256color|screen-256color|eterm-color|rxvt*)
            color_prompt=yes;;
        dumb)
            color_prompt=no;;
    esac

    PROMPT_COLOR="1;32m"

    if [ "$color_prompt" = yes ]; then
        PS1='\n\[\033[$PROMPT_COLOR\][\u@\h:\w]\$ \[\033[0m\]'
    else
        PS1='\n[\u@\h:\w]\$ '
    fi
    unset color_prompt

    if [ -f "$HOME/.bash_aliases" ]; then
        . "$HOME/.bash_aliases"
    fi

    alias ls="ls -G"

    if [ -d "$HOME/.nix-profile/Applications/Emacs.app/" ]; then
        alias Emacs.app="$HOME/.nix-profile/Applications/Emacs.app/Contents/MacOS/Emacs"
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
