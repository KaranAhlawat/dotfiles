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

#zinit light romkatv/powerlevel10k
zinit light agkozak/zsh-z
zinit light supercrabtree/k

VIM_MODE_VICMD_KEY='jj'
zinit load softmoth/zsh-vim-mode
# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
#[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
eval "$(starship init zsh)"

# ----------------------------- ALIASES ---------------------------------
alias ls="exa --icons"
alias l="ls -la"

alias vi="nvim"
alias vim="nvim"

# ----------------------------- EXPORTS --------------------------------
export EDITOR="nvim"

# dvm
export DENO_INSTALL="$HOME/.deno"
export PATH=/home/karan/.deno/bin:$PATH

# fnm
export PATH=/home/karan/.local/bin/:/home/karan/.fnm:/home/karan/.yarn/bin/:$PATH
eval "`fnm env`"

# scripts
export PATH=/home/karan/.config/scripts/:$PATH

export PATH=/usr/lib/elixir-ls/:$PATH

#export CLASSPATH="/home/karan/.java/eclipse.jdt.ls/org.eclipse.jdt.ls.product/target/repository/plugins/org.eclipse.equinox.launcher_1.5.700.v20200207-2156.jar"

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"
