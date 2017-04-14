# rbenv
export PATH=$HOME/.rbenv/bin:$PATH
eval "$(rbenv init - zsh)"

# pyenv
if which pyenv > /dev/null; then eval "$(pyenv init -)"; fi
eval "$(pyenv virtualenv-init -)"

# TeX
export PATH=/Applications/TeXLive/Library/texlive/2013/bin/x86_64-darwin:$PATH

# Golang
export GOPATH=$HOME/.go
export PATH=$PATH:$GOPATH/bin:/usr/local/go/bin

# phpenv
export PATH="/Users/Kouta/.phpenv/bin:$PATH"
eval "$(phpenv init -)"

# nvm
export NVM_DIR="/Users/Kouta/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm

# cask
export PATH="/Users/Kouta/.cask/bin:$PATH"

# powerline
export PATH="/Users/Kouta/.local/bin:$PATH"
