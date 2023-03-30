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

#export PATH="$HOME/.yarn/bin:$PATH"
#export GRAVEYARD="$HOME/.local/share/graveyard"
