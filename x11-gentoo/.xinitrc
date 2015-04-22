#! /usr/bin/env sh

exec > ~/.xsession-errors 2>&1

if test -z "$DBUS_SESSION_BUS_ADDRESS"; then
    eval `dbus-launch --sh-syntax --exit-with-session`
fi

if test -x /usr/bin/VBoxClient-all; then
    VBoxClient-all
fi

if test -f "$HOME/.Xresources"; then
    xrdb -merge $HOME/.Xresources
fi

if test `hostname` = "thaumas"; then
    xmodmap ~/etc/x11-gentoo/xmodmaprc-thaumas
fi

xsetroot -cursor_name left_ptr

xset b off

unclutter -idle 1 &

compton -bc

hsetroot -solid "#5f5f5f"

eval `ssh-agent -s`

if test ! `pgrep -u "$USER" -f "emacs --daemon"`; then
    emacs --daemon
fi

exec xmonad
