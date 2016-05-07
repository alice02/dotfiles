# rbenv
export PATH=$HOME/.rbenv/bin:$PATH
eval "$(rbenv init - zsh)"

# pyenv
if which pyenv > /dev/null; then eval "$(pyenv init -)"; fi

# TeX
export PATH=/Applications/TeXLive/Library/texlive/2013/bin/x86_64-darwin:$PATH

# Golang
export GOPATH=$HOME/.go
export PATH=$PATH:$GOPATH/bin