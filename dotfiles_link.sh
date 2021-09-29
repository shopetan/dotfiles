#!/bin/sh
ln -sf ~/src/github.com/shopetan/dotfiles/wifi /usr/local/bin/wifi
ln -sf ~/src/github.com/shopetan/dotfiles/.tmux.conf ~/.tmux.conf
ln -sf ~/src/github.com/shopetan/dotfiles/spacemacs/.spacemacs ~/.spacemacs
ln -sf ~/src/github.com/shopetan/dotfiles/git/.gitconfig ~/.gitconfig
ln -sf ~/src/github.com/shopetan/dotfiles/zsh/.zshrc ~/.zshrc
ln -sf ~/src/github.com/shopetan/dotfiles/zsh/.zshenv ~/.zshenv
ln -sf ~/src/github.com/shopetan/dotfiles/.upgrade-go ~/.upgrade-go

# change shell
chsh -s $(which zsh)

source ~/.zshrc
