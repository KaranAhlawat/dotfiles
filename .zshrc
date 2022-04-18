# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# ----------------------------------- ZINIT ------------------------------------
#
### Load up zinit
if [[ ! -f $HOME/.zinit/bin/zinit.zsh ]]; then
    print -P "%F{33}▓▒░ %F{220}Installing %F{33}DHARMA%F{220} Initiative Plugin Manager (%F{33}zdharma/zinit%F{220})…%f"
    command mkdir -p "$HOME/.zinit" && command chmod g-rwX "$HOME/.zinit"
    command git clone https://github.com/zdharma-continuum/zinit "$HOME/.zinit/bin" && \
        print -P "%F{33}▓▒░ %F{34}Installation successful.%f%b" || \
        print -P "%F{160}▓▒░ The clone has failed.%f%b"
fi

source "$HOME/.zinit/bin/zinit.zsh"
autoload -Uz _zinit compinit
(( ${+_comps} )) && _comps[zinit]=_zinit
fpath+=~/.config/zsh/completions/_fnm
compinit

# Load a few important annexes, without Turbo
# (this is currently required for annexes)
zinit light-mode for \
    zdharma-continuum/z-a-rust \
    zdharma-continuum/z-a-as-monitor \
    zdharma-continuum/z-a-patch-dl \
    zdharma-continuum/z-a-bin-gem-node

zinit light zsh-users/zsh-syntax-highlighting
zinit light zsh-users/zsh-completions

zinit light agkozak/zsh-z
zinit light supercrabtree/k

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
#[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
eval "$(starship init zsh)"

# ----------------------------- ALIASES ---------------------------------
alias ls="exa --icons"
alias l="ls -la"

alias vi="nvim"
alias vim="nvim"

alias gs="git status"
alias ga="git add"

# ----------------------------- EXPORTS --------------------------------
export EDITOR="emacsclient"
export PATH=/home/karan/.local/bin:$PATH

# coursier
export PATH="$PATH:/home/karan/.local/share/coursier/bin"

# intellij from toolbox
export PATH="$PATH:/home/karan/programs/idea-IC-213.6777.52/bin"

# Go path
export PATH="$PATH:/home/karan/go/bin"

# GO Path
export PATH="$PATH:/usr/local/go/bin"
