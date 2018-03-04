#! /usr/bin/env sh

CONKYRC="$HOME/etc/conky/conkyrc-$(hostname)"

if test -f "$HOME/.zprofile"; then
    source "$HOME/.zprofile"
fi

if test $(hostname) = "thaumas"; then
    xmodmap "$HOME/etc/x11/xmodmaprc-thaumas"
fi

xsetroot -solid "#222222"

xset b off

xrdb -load "$HOME/.Xresources"

if test -f $CONKYRC; then
    (conky -c $CONKYRC | while read LINE; do xsetroot -name "$LINE"; done) &
fi

export _JAVA_AWT_WM_NONREPARENTING=1

exec dbus-launch --auto-syntax --exit-with-session dwm
