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

eval "$(zoxide init bash)"
eval "$(starship init bash)"

# Scala
export PATH="$PATH:$HOME/.local/share/coursier/bin"

# pyenv
export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"

# fnm
export PATH="/home/karan/.local/share/fnm:$PATH"
eval "$(fnm env --use-on-cd)"
