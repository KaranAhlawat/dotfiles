# .bash_profile

# User specific environment
if ! [[ "$PATH" =~ "$HOME/.local/bin:" ]]; then
	PATH="$HOME/.local/bin:$PATH"
fi

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export LC_ALL=en_IN.UTF-8
export LANG=en_IN.UTF-8
export LSP_USE_PLISTS=true

# Flatpak
export PATH="$PATH:$XDG_DATA_HOME/flatpak/exports/bin"

# cargo
export CARGO_HOME="$XDG_DATA_HOME/cargo"
export PATH="$PATH:$CARGO_HOME/bin"
. "/home/karan/.local/share/cargo/env"

# pnpm
export PNPM_HOME="/home/karan/.local/share/pnpm"
case ":$PATH:" in
*":$PNPM_HOME:"*) ;;
*) export PATH="$PNPM_HOME:$PATH" ;;
esac

export PATH="$PATH:$HOME/.bun/bin"

export DOCKER_HOST="unix:///run/user/1000/docker.sock"

# Added by Toolbox App
export PATH="$PATH:/home/karan/.local/share/JetBrains/Toolbox/scripts"

# NVIM
export PATH="$PATH:/opt/nvim-linux64/bin"

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

# Non-interactive setup for Mise shims
if [ -z "$PS1" ]; then
	eval "$(mise activate --shims)"
fi

# opam configuration
test -r /home/karan/.opam/opam-init/init.sh && . /home/karan/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true
