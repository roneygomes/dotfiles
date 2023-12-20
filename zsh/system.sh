function uuid() {
  uuid_string=$(uuidgen | awk '{print tolower($0)}')
  printf "%s" "$uuid_string" | pbcopy
  echo "$uuid_string"
}

function vpn() {
  eval $(op signin)

  user=$(op item get iFood --fields username)
  pass=$(op item get iFood --fields password)
  otp=$(op item get --otp iFood)

  ifood-vpn-login -u $user -p $pass -o --otp-code $otp
}

function token() {
  eval $(op signin)

  user=$(op item get iFood --fields username)
  pass=$(op item get iFood --fields password)
  otp=$(op item get --otp iFood)

  ifood-req-token -u $user -p $pass -o --otp-code $otp -j {value}
}

function token-var() {
  export REQ_TOKEN=$(req-token)
  echo '~*~ jwt token set to $REQ_TOKEN var ~*~'
}

alias reload="source ~/.zshrc"
alias zshconfig="vim ~/.zshrc"
alias ohmyzsh="vim ~/.oh-my-zsh"
alias ip="ifconfig | sed -En 's/127.0.0.1//;s/.*inet (addr:)?(([0-9]*\.){3}[0-9]*).*/\2/p'"

export LC_ALL=en_US.UTF-8

# SSL
export PATH="/usr/local/opt/openssl@3/bin:$PATH"
export LDFLAGS="-L/usr/local/opt/openssl@3/lib"
export CPPFLAGS="-I/usr/local/opt/openssl@3/include"
export PKG_CONFIG_PATH="$(brew --prefix)/opt/cyrus-sasl/lib/pkgconfig"
export OPENSSL_ROOT_DIR="$(brew --prefix)/opt/openssl@3"
