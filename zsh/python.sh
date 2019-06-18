PATH=$PATH:~/Library/Python/3.7/bin

alias python=python3
alias pip=pip3

export WORKON_HOME=~/.venv
export VIRTUALENVWRAPPER_PYTHON=/usr/local/bin/python3

mkdir -p ${WORKON_HOME}

source /usr/local/bin/virtualenvwrapper.sh