function uuid() {
  uuid_string=$(uuidgen | awk '{print tolower($0)}')
  printf $uuid_string | pbcopy
  echo "$uuid_string"
}


alias reload="source ~/.zshrc"
alias zshconfig="vim ~/.zshrc"
alias ohmyzsh="vim ~/.oh-my-zsh"
alias ip="ifconfig | sed -En 's/127.0.0.1//;s/.*inet (addr:)?(([0-9]*\.){3}[0-9]*).*/\2/p'"

export FZF_DEFAULT_COMMAND='rg --files --no-ignore-vcs --hidden'
export LC_ALL=en_US.UTF-8
