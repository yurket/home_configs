#!/bin/bash

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/Xresources
sysmodmap=/etc/X11/xinit/Xmodmap

BG_IMAGES_PATH=$HOME/bg_images

for f in /usr/share/fonts/*
do
    echo "adding fontdir '$f'"
    xset +fp "$f"
done
xset fp rehash

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi


exec xbindkeys &
exec feh --bg-scale $HOME/bg_images/black.jpg &
exec xmms2 &

exec xmonad > $HOME/.xmonad/log.txt 2>&1
