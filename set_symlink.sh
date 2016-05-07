# カレントディレクトリを確認
current_dir=$(pwd)

# oh-my-zsh関連
# .zshrcのsymlink
ln -snf ${current_dir}/.oh-my-zsh/.zshrc ~/.zshrc
# ~/.oh-my-zsh/custom/以下のファイルたちのsymlink
files=${current_dir}/.oh-my-zsh/custom/*
for filepath in $files; do
    ln -snf ${filepath} ~/.oh-my-zsh/custom/
done


# emacs関連
ln -snf ${current_dir}/.emacs.d/Cask ~/.emacs.d/Cask
ln -snf ${current_dir}/.emacs.d/init.el ~/.emacs.d/init.el
