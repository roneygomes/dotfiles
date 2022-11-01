# pyenv is used for managing multiple python versions in the system
# it allows us to install and switch between different python versions when required
export PYENV_ROOT=$HOME/.pyenv

export PATH=$PYENV_ROOT/shims:$PATH
export PYTHON_CONFIGURE_OPTS="--enable-framework"

eval "$(pyenv init -)"

# virtualenvwrapper is a tool used for managing python virtual environments, which is a mechanism for
# isolating project dependencies. that said, we can have multiple python versions each containing
# many virtual environments
export WORKON_HOME=~/.venv
mkdir -p ${WORKON_HOME}

PYTHON_VERSION=$(cat "$HOME"/.pyenv/version)

if [ -n "$PYTHON_VERSION" ]; then
  export PATH=$PYENV_ROOT/versions/$PYTHON_VERSION/bin:$PATH
fi

VIRTUAL_ENV_WRAPPER="$HOME"/.pyenv/versions/"$PYTHON_VERSION"/bin/virtualenvwrapper.sh

if [ -f "$VIRTUAL_ENV_WRAPPER" ]; then
  # shellcheck disable=SC1090
  source "$VIRTUAL_ENV_WRAPPER"
fi
