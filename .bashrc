# xmonad on arch linux

export _JAVA_AWT_WM_NONREPARENTING=1

[[ ! $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx -- -dpi 100
