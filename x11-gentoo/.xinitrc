#! /usr/bin/env sh

exec > ~/.xsession-errors 2>&1

[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

[[ $(hostname) == "thaumas" ]] && xmodmap ~/etc/x11-gentoo/xmodmaprc-thaumas

xsetroot -cursor_name left_ptr

xset b off

unclutter -idle 1 &

compton -bc

hsetroot -solid "#5f5f5f"

[[ $(hostname) == "thalassa" && -e /usr/bin/VBoxClient-all ]] && VBoxClient-all

eval `ssh-agent`

emacs --daemon

exec dbus-launch --sh-syntax --exit-with-session xmonad
