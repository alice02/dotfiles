# 履歴検索: C-p,n で検索・補完
autoload -U history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end

# bindkey '^P' history-beginning-search-backward
# bindkey '^N' history-beginning-search-forward