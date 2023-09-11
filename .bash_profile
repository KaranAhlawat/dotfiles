# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

# User specific environment
if ! [[ "$PATH" =~ "$HOME/.local/bin:" ]]
then
    PATH="$HOME/.local/bin:$PATH"
fi

export XDG_CONFIG_HOME="$HOME/.config"
export LC_ALL=en_IN.UTF-8
export LANG=en_IN.UTF-8 

eval "$(zoxide init bash)"
eval "$(starship init bash)"

# Zig
export PATH="$PATH:$HOME/.local/share/zig"

export PATH="$PATH:$HOME/repos/vcpkg"

# pyenv
export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

# fnm
export PATH="/home/karan/.local/share/fnm:$PATH"
eval "$(fnm env --use-on-cd)"

# cargo
export PATH="$PATH:$HOME/.cargo/bin"

# pnpm
export PNPM_HOME="/home/karan/.local/share/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac

# Haskell
[ -f "/home/karan/.ghcup/env" ] && source "/home/karan/.ghcup/env" #ghcup-env
