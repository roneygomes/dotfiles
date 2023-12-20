alias python="python3"
alias pip="pip3"

export WORKON_HOME=~/.venv
export VIRTUALENVWRAPPER_PYTHON=$(which python3)

mkdir -p ${WORKON_HOME}

VIRTUAL_ENV_WRAPPER_PATH=$(which virtualenvwrapper.sh)

if [ -f "$VIRTUAL_ENV_WRAPPER_PATH" ]; then
  source "$VIRTUAL_ENV_WRAPPER_PATH"
fi

PYTHON_VERSION=$(python3 --version | awk '{print $2}' | cut -d. -f1,2)
PATH=$PATH:~/Library/Python/$PYTHON_VERSION/bin

