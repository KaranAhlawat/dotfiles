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

eval "$(zoxide init bash)"
eval "$(starship init bash)"

# conditionals
if [ -f "/run/.toolboxenv" ]
then
  . "$HOME/.cargo/env"
  eval "$(fnm env --use-on-cd)"
  export ANDROID_SDK="$HOME/Android/Sdk"
  export PATH="$PATH:/opt/flutter/bin"
  export PATH="$PATH:$ANDROID_SDK/emulator:$ANDROID_SDK/platform-tools:$ANDROID_SDK/cmdline-tools/latest/bin"
fi
