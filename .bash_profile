# bash for arch linux, fish for mac os.

export EMAIL=ysmiraak@gmail.com
export ALTERNATE_EDITOR=emacs
export EDITOR=emacsclient
export VISUAL=emacsclient

export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/opt/intel/lib/intel64:/opt/intel/mkl/lib/intel64

[[ -f ~/.bashrc ]] && . ~/.bashrc

if [ -z "$DISPLAY" ] && [ -n "$XDG_VTNR" ] && [ "$XDG_VTNR" -eq 1 ]; then
    exec startx
fi
