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

# Scala
export PATH="$PATH:$HOME/.local/share/coursier/bin"

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

# opam configuration
test -r /home/karan/.opam/opam-init/init.sh && . /home/karan/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true

# roswell
export PATH="$PATH:$HOME/.roswell/bin"

# bun
export BUN_INSTALL="$HOME/.bun"
export PATH=$BUN_INSTALL/bin:$PATH
