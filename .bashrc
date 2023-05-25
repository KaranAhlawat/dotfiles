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
alias dcr='docker compose run --rm'
alias sail='[ -f sail ] && sh sail || sh vendor/bin/sail'

# inside toolboxes
if [ -f "/run/.toolboxenv" ]
then
	alias ts='tree-sitter'
	alias sail='[ -f sail ] && sh sail || sh vendor/bin/sail'

	# >>> mamba initialize >>>
	# !! Contents within this block are managed by 'mamba init' !!
	export MAMBA_EXE="/usr/local/bin/micromamba";
	export MAMBA_ROOT_PREFIX="/var/home/karan/.micromamba";
	__mamba_setup="$("$MAMBA_EXE" shell hook --shell bash --prefix "$MAMBA_ROOT_PREFIX" 2> /dev/null)"
	if [ $? -eq 0 ]; then
	    eval "$__mamba_setup"
	else
	    if [ -f "/var/home/karan/.micromamba/etc/profile.d/micromamba.sh" ]; then
		. "/var/home/karan/.micromamba/etc/profile.d/micromamba.sh"
	    else
		export  PATH="/var/home/karan/.micromamba/bin:$PATH"  # extra space after export prevents interference from conda init
	    fi
	fi
	unset __mamba_setup
	# <<< mamba initialize <<<
	
	micromamba activate base
fi

# completions
COMPLETIONS_DIR="$HOME/.bash_completions"

