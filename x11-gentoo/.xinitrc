#! /usr/bin/env sh

exec > ~/.xsession-errors 2>&1

[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

xsetroot -cursor_name left_ptr

xset b off

unclutter -idle 1 &

compton -bc

hsetroot -solid "#333344"

VBoxClient-all

emacs --daemon

exec dbus-launch --sh-syntax --exit-with-session xmonad
