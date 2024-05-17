path+=("$HOME/.local/bin")

XDG_CONFIG_HOME="$HOME/.config"
XDG_DATA_HOME="$HOME/.local/share"
LC_ALL=en_IN.UTF-8
LANG=en_IN.UTF-8
LSP_USE_PLISTS=true

CARGO_HOME="$XDG_DATA_HOME/cargo"
path+=("$CARGO_HOME/bin")

PNPM_HOME="$XDG_DATA_HOME/pnpm"
path+=("$PNPM_HOME")

path+=("$HOME/.bun/bin")

DOCKER_HOST="unix:///run/user/1000/docker.sock"

path+=("$XDG_DATA_HOME/JetBrains/Toolbox/scripts")

path+=("/opt/nvim-linux64/bin")

if [ -z "$PS1" ]; then
	eval "$(mise activate --shims)"
fi

test -r /home/karan/.opam/opam-init/init.sh && . /home/karan/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true
