export PYENV_ROOT=$HOME/.pyenv
export PATH=$PYENV_ROOT/bin:$PATH
export PATH=$PYENV_ROOT/shims:$PATH

export PYTHON_CONFIGURE_OPTS="--enable-framework"
export WORKON_HOME=~/.venv

mkdir -p ${WORKON_HOME}

eval "$(pyenv init -)"
source "$HOME"/.pyenv/versions/"$(cat ~/.pyenv/version)"/bin/virtualenvwrapper.sh
