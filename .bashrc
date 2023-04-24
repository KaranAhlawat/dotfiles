# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# Aliases
alias ls='exa --icons'
alias l='ls -la'
alias open='xdg-open'
alias lfont='pango-list | rg -S'

# inside toolboxes
if [ -f "/run/.toolboxenv" ]
then
	alias ts='tree-sitter'
	alias sail='[ -f sail ] && sh sail || sh vendor/bin/sail'
	alias vi='hx'
fi

# completions
COMPLETIONS_DIR="$HOME/.bash_completions"
