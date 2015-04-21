#! /usr/bin/env sh

[[ -f $HOME/.zprofile ]] && source $HOME/.zprofile

xsetroot -cursor_name left_ptr

xset b off

compton -bc

hsetroot -solid "#5f5f5f"

unclutter -idle 1 &

emacs --daemon

exec dbus-launch --sh-syntax --exit-with-session xmonad
