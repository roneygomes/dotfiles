export WORKON_HOME=~/.venv
export PYTHON_CONFIGURE_OPTS="--enable-framework"

mkdir -p ${WORKON_HOME}

eval "$(pyenv init -)"
source ~/.pyenv/versions/$(cat ~/.pyenv/version)/bin/virtualenvwrapper.sh
