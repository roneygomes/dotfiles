export WORKON_HOME=~/.venv
export VIRTUALENVWRAPPER_PYTHON

mkdir -p ${WORKON_HOME}

VIRTUALENVWRAPPER_PYTHON=$(which python3)
VIRTUAL_ENV_WRAPPER_PATH=$(which virtualenvwrapper.sh)

if [ -f "$VIRTUAL_ENV_WRAPPER_PATH" ]; then
  # shellcheck disable=SC1090
  source "$VIRTUAL_ENV_WRAPPER_PATH"
fi

PYTHON_VERSION=$(python3 --version | awk '{print $2}' | cut -d. -f1,2)
PATH=$PATH:~/Library/Python/$PYTHON_VERSION/bin

alias python="python3"

function pip() {
  python -m pip "$@"
}
