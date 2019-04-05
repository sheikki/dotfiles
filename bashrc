# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

shopt -s histappend
shopt -s globstar
#export PS1="🦄 \[$(tput bold; tput setaf 6)\]\w \[$(tput sgr0)\]"
export PS1="\[$(tput bold; tput setaf 2)\]@\h \[$(tput bold; tput setaf 6)\]\w \[$(tput sgr0)\]"
export TERM=xterm-256color
eval `dircolors ~/.dir_colors`
