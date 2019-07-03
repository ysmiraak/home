# bash for arch linux

export EMAIL=ysmiraak@gmail.com
export ALTERNATE_EDITOR=emacs
export EDITOR=emacsclient
export VISUAL=emacsclient
export PATH=$HOME/.local/bin:$PATH

export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/opt/cuda/lib64:/opt/cuda/extras/CUPTI/lib64
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/opt/intel/mkl/lib/intel64
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/opt/intel/lib/intel64

export NODE_PATH=/usr/lib/node_modules

[[ -f ~/.bashrc ]] && . ~/.bashrc

[[ ! $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx -- -dpi 100
