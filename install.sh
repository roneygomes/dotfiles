#!/bin/bash

if [ ! -d "$HOME/code/projects/work" ]; then
  mkdir -p "$HOME"/code/projects/work
fi

if [ ! -d "$HOME/code/projects/personal" ]; then
  mkdir -p "$HOME"/code/projects/personal
fi

if [ ! -d "$HOME/code/tools" ]; then
  mkdir -p "$HOME"/code/tools
fi

if [ ! -f "$HOME/.gitconfig" ]; then
  ln -s "$HOME"/code/projects/personal/dotfiles/git/gitconfig "$HOME"/.gitconfig
fi

if [ ! -f "$HOME/.vimrc" ]; then
  ln -s "$HOME"/code/projects/personal/dotfiles/vim/vimrc "$HOME"/.vimrc
fi

if [ ! -f "$HOME/.zshrc" ]; then
  ln -s "$HOME"/code/projects/personal/dotfiles/zsh/zshrc "$HOME"/.zshrc
fi

if [ ! -x "$(command -v pyenv)" ]; then
    brew install pyenv
fi

if [ ! -x "$(command -v rbenv)" ]; then
    brew install rbenv
fi

if [ ! -x "$(command -v kubectl)" ]; then
    brew install kubectl
fi
