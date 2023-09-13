# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

export LC_ALL=en_IN.UTF-8
export LANG=en_IN.UTF-8

# Aliases
alias ls='exa'
alias l='ls -alG'
alias open='xdg-open'
alias lfont='pango-list | rg -S'
alias dcr='docker compose run --rm'
alias mill='mill --disable-ticker'
alias ts='tree-sitter'
alias bat='batcat'
alias fd='fdfind'
alias tmux='tmux -u'
alias docker='podman'

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

COMPLETIONS_DIR="$HOME/.bash_completions"
if [ -d $COMPLETIONS_DIR ]; then
	. "$COMPLETIONS_DIR/cmake"
fi

source /home/karan/repos/vcpkg/scripts/vcpkg_completion.bash

if command -v tmux &> /dev/null && [ -n "$PS1" ] && [[ ! "$TERM" =~ screen ]] && [[ ! "$TERM" =~ dumb ]] && [[ ! "$TERM" =~ tmux ]] && [ -z "$TMUX" ]; then
  exec tmux
fi
