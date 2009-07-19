# Check for an interactive session
[ -z "$PS1" ] && return
alias pac='sudo pacman-color'
alias ls='ls --color=auto'
alias cd..='cd ..'
alias grep='grep --color=auto'
export GREP_COLOR="1;33"
eval `dircolors -b`
PS1='\[\033[0;32m\]&#9484;&#9472;[ \[\033[0m\033[0;32m\]\u\[\033[0m\] @ \[\033[0;36m\]\h\[\033[0m\033[0;32m\] ] - [ \[\033[0m\]\t \d\[\033[0;32m\] ] - [ \[\033[0m\]\w\[\033[0;32m\] ]\n\[\033[0;32m\]&#9492;&#9472;[\[\033[0m\033[0;32m\]\$\[\033[0m\033[0;32m\]]>\[\033[0m\] '
PS2='>'
cd() {
  if [ -n "$1" ]; then
    builtin cd "$@" && ls
  else
    builtin cd ~ && ls
  fi
}
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi
