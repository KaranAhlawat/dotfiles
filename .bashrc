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
alias mill='mill --disable-ticker'

# inside toolboxes
if [ -f "/run/.toolboxenv" ]
then
	alias ts='tree-sitter'
	alias vi='nvim'
	alias sail='[ -f sail ] && sh sail || sh vendor/bin/sail'

	# opam configuration
	test -r /var/home/karan/.opam/opam-init/init.sh && . /var/home/karan/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true

	#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
	export SDKMAN_DIR="$HOME/.sdkman"
	[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

	export PYENV_ROOT="$HOME/.pyenv"
	command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
	eval "$(pyenv init -)"
	eval "$(pyenv virtualenv-init -)"
fi

# completions
COMPLETIONS_DIR="$HOME/.bash_completions"

