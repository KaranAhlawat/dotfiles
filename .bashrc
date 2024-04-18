# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

export LC_ALL=en_IN.UTF-8
export LANG=en_IN.UTF-8

# Aliases
alias ls='eza'
alias l='ls -alG'
alias open='xdg-open'
alias lfont='pango-list | rg -S'
alias bat='batcat --paging=never'
alias cat='batcat --paging=never'
alias fd='fdfind'
alias tmux='tmux -u'
alias apt='nala'
alias vim='nvim'

COMPLETIONS_DIR="$HOME/.bash_completions"
if [ -d $COMPLETIONS_DIR ]; then
	. "$COMPLETIONS_DIR/mise"
fi

if command -v tmux &>/dev/null && [ -n "$PS1" ] && [[ ! "$TERM" =~ screen ]] && [[ ! "$TERM" =~ dumb ]] && [[ ! "$TERM_PROGRAM" =~ vscode ]] && [[ ! "$TERM" =~ tmux ]] && [ -z "$TMUX" ]; then
	exec tmux
fi

[ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
  source "$EAT_SHELL_INTEGRATION_DIR/bash"

eval "$(mise activate bash)"
eval "$(mise hook-env -s bash)"

source <(jj util completion bash)
eval "$(zoxide init --cmd cd bash)"
eval "$(starship init bash)"
