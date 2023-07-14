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

function get_host() {
    if [ -f "/run/.toolboxenv" ]
    then
        TOOLBOX_NAME=$(cat /run/.containerenv | grep -oP "(?<=name=\")[^\";]+")
        echo "${TOOLBOX_NAME}"
    else
       echo "${HOSTNAME}"
    fi
}

export PS1="\n\[\e]0;\u@\h: \w\a\]\[\033[01;32m\]\u@\`get_host\`\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\n\$ "
export XDG_CONFIG_HOME="$HOME/.config"

eval "$(zoxide init bash)"
eval "$(starship init bash)"

# conditionals
if [ -f "/run/.toolboxenv" ]
then
  # RUST
  . "$HOME/.cargo/env"

  # NODE
  eval "$(fnm env --use-on-cd)"
  
  # Scala
  export PATH="$PATH:$HOME/.local/share/coursier/bin"

  # pyenv
  export PYENV_ROOT="$HOME/.pyenv"
  command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
  eval "$(pyenv init -)"
fi
