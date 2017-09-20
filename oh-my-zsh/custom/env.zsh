# anyenv
export PATH="$HOME/.anyenv/bin:$PATH"
eval "$(anyenv init --no-rehash - zsh)"

# pyenv
eval "$(pyenv virtualenv-init - zsh)"

# Golang binary
export PATH="$HOME/bin:$PATH"
export GOPATH="$HOME"
export GOROOT=`go env GOROOT`

# Cask
export PATH="$HOME/.cask/bin:$PATH"

# for powerline
export PATH="$HOME/.local/bin:$PATH"
