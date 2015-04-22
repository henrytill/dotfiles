#! /usr/bin/env sh

if test -z "$DBUS_SESSION_BUS_ADDRESS"; then
    eval `dbus-launch --sh-syntax --exit-with-session`
fi

if test -f "$HOME/.zprofile"; then
    source $HOME/.zprofile
fi

xsetroot -cursor_name left_ptr

xset b off

compton -bc

hsetroot -solid "#5f5f5f"

unclutter -idle 1 &

if test ! `pgrep -u $USER -f "emacs --daemon"`; then
    emacs --daemon
fi

exec xmonad
