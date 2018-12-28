#!/bin/sh

# merge Xresources
[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources
# merge xmodmap
[[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap
# source scripts
if [ -d /etc/X11/xinit/xinitrc.d ] ; then
    for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

hsetroot -solid "#000000"
xcompmgr &
exec xmonad
