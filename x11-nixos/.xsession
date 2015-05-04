#! /usr/bin/env sh

if test -z "$DBUS_SESSION_BUS_ADDRESS"; then
    eval `dbus-launch --sh-syntax --exit-with-session`
fi

if test -f "$HOME/.zprofile"; then
    source "$HOME/.zprofile"
fi

xsetroot -solid "#5f5f5f"

xset b off

if test ! `pgrep -u $USER -f "emacs --daemon"`; then
    emacs --daemon
fi

conky -c "$HOME/etc/conky/conkyrc-basic" | while read LINE; do xsetroot -name "$LINE"; done &

exec dwm
