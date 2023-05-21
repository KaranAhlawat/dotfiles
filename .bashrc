# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# Aliases
alias ls='exa'
alias l='ls -la'
alias open='xdg-open'
alias lfont='pango-list | rg -S'
alias dcr='docker-compose run --rm'

# inside toolboxes
if [ -f "/run/.toolboxenv" ]
then
	alias ts='tree-sitter'
	alias sail='[ -f sail ] && sh sail || sh vendor/bin/sail'
	alias vi='zile'
fi

# completions
COMPLETIONS_DIR="$HOME/.bash_completions"