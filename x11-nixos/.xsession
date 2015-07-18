#! /usr/bin/env sh

if test -z "$DBUS_SESSION_BUS_ADDRESS"; then
    eval `dbus-launch --sh-syntax --exit-with-session`
fi

if test -f "$HOME/.zprofile"; then
    source "$HOME/.zprofile"
fi

if test `hostname` = "thaumas"; then
    xmodmap "$HOME/etc/x11/xmodmaprc-thaumas"
fi

xsetroot -solid "#5f5f5f"

xset b off

export _JAVA_AWT_WM_NONREPARENTING=1

exec dwm
