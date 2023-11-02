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
export XDG_DATA_HOME="$HOME/.local/share"
export LC_ALL=en_IN.UTF-8
export LANG=en_IN.UTF-8 

eval "$(zoxide init bash)"
eval "$(starship init bash)"

# Flatpak
export PATH="$PATH:$XDG_DATA_HOME/flatpak/exports/bin"

# Zig
export PATH="$PATH:$HOME/.local/share/zig"

export PATH="$PATH:$HOME/repos/vcpkg"

# fnm
export PATH="/home/karan/.local/share/fnm:$PATH"
eval "$(fnm env --use-on-cd)"

# cargo
export CARGO_HOME="$XDG_DATA_HOME/cargo"
export PATH="$PATH:$CARGO_HOME/bin"

# pnpm
export PNPM_HOME="/home/karan/.local/share/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac

# >>> coursier install directory >>>
export PATH="$PATH:/home/karan/.local/share/coursier/bin"
# <<< coursier install directory <<<
#
. "/home/karan/.local/share/cargo/env"

# >>> mamba initialize >>>
# !! Contents within this block are managed by 'mamba init' !!
export MAMBA_EXE='/home/karan/.local/bin/micromamba';
export MAMBA_ROOT_PREFIX='/home/karan/.local/share/micromamba';
__mamba_setup="$("$MAMBA_EXE" shell hook --shell bash --root-prefix "$MAMBA_ROOT_PREFIX" 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__mamba_setup"
else
    alias micromamba="$MAMBA_EXE"  # Fallback on help from mamba activate
fi
unset __mamba_setup
# <<< mamba initialize <<<

# Created by `pipx` on 2023-09-30 16:42:25
export PATH="$PATH:/home/karan/.local/bin"
