#!/bin/sh
ln -sf ~/.ghq/github.com/shopetan/dotfiles/wifi /usr/local/bin/wifi
ln -sf ~/.ghq/github.com/shopetan/dotfiles/.tmux.conf ~/.tmux.conf
ln -sf ~/.ghq/github.com/shopetan/dotfiles/spacemacs/.spacemacs ~/.spacemacs
ln -sf ~/.ghq/github.com/shopetan/dotfiles/git/.gitconfig ~/.gitconfig
ln -sf ~/.ghq/github.com/shopetan/dotfiles/zsh/.zshrc ~/.zshrc
ln -sf ~/.ghq/github.com/shopetan/dotfiles/zsh/.zshenv ~/.zshenv

# change shell
chsh -s $(which zsh)

source ~/.ghq/github.com/shopetan/dotfiles/zsh/.zshrc
