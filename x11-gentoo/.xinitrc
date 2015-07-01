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
    conkyrc="$HOME/etc/conky/conkyrc-thaumas"
    xmodmap "$HOME/etc/x11/xmodmaprc-thaumas"
fi

xsetroot -solid "#5f5f5f"

xset b off

export _JAVA_AWT_WM_NONREPARENTING=1

eval `ssh-agent -s`

if test ! `pgrep -u "$USER" -f "emacs --daemon"`; then
    emacs --daemon
fi

conky -c "${conkyrc:-$HOME/etc/conky/conkyrc-basic}" | while read LINE; do xsetroot -name "$LINE"; done &

exec dwm
