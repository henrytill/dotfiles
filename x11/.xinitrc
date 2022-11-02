#!/bin/sh

xrdb -load $HOME/.Xresources

xsetroot -solid gray &

setxkbmap -option ctrl:nocaps &

xterm -g 100x48+10+10 &

exec cwm
