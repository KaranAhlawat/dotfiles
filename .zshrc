# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
# if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
#   source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
# fi

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

_dotnet_zsh_complete()
{
  local completions=("$(dotnet complete "$words")")

  reply=( "${(ps:\n:)completions}" )
}

compctl -K _dotnet_zsh_complete dotnet

source "$HOME/.zinit/bin/zinit.zsh"
autoload -Uz _zinit compinit
(( ${+_comps} )) && _comps[zinit]=_zinit
compinit
zstyle ':completion:*' menu select

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

eval "$(starship init zsh)"

# ----------------------------- ALIASES ---------------------------------
alias ls="exa --icons"
alias l="ls -la"

alias vi="nvim"
alias vim="nvim"

alias gs="git status"
alias ga="git add"

alias tree="exa --tree --icons"

alias code="codium"

# ----------------------------- EXPORTS --------------------------------
export EDITOR="nvim"
export PATH=/home/karan/.local/bin:$PATH

# coursier
export PATH="$PATH:/home/karan/.local/share/coursier/bin"

# GO Path
export PATH="$PATH:/usr/local/go/bin"
export PATH="$PATH:/home/karan/go/bin"

# nvm setup
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# Cargo
export PATH="$PATH:/home/karan/.cargo/bin"

# Nim
export PATH=/home/karan/.nimble/bin:$PATH

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

# pnpm
export PNPM_HOME="/home/karan/.local/share/pnpm"
export PATH="$PNPM_HOME:$PATH"
# pnpm end
# bun completions
[ -s "/home/karan/.bun/_bun" ] && source "/home/karan/.bun/_bun"

# Bun
export BUN_INSTALL="/home/karan/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"
