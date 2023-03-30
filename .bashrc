# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# Aliases
alias ls='exa --icons'
alias l='ls -la'
alias ts='tree-sitter'
alias open='xdg-open'
alias lfont='pango-list | rg -S'
alias sail='[ -f sail ] && sh sail || sh vendor/bin/sail'

# completions
COMPLETIONS_DIR="$HOME/.bash_completions"
