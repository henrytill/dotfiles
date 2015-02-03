#!/usr/bin/env bash

xsetroot -solid "black" -cursor_name left_ptr

xset b off

unclutter -idle 1 &

emacs --daemon

exec dbus-launch --auto-syntax --exit-with-session xmonad
