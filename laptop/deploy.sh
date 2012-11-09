#! /bin/bash

if [ -L $0 ] ; then
    DIR=$(dirname $(readlink -f $0)) ;
else
    DIR=$(dirname $0) ;
fi ;

#backup folder
BACKUP_FOLDER="$HOME/bakconf_`date +%Y%m%d`"
mkdir -v $BACKUP_FOLDER

#bashrc
if [ -f ~/.bashrc ]; then
  cp -v ~/.bashrc $BACKUP_FOLDER/.bashrc
fi
cp -vf $DIR/.bashrc $HOME/.bashrc

#vimrc
if [ -f ~/.vimrc ]; then
  cp -v ~/.vimrc $BACKUP_FOLDER/.vimrc
fi
cp -vf $DIR/.vimrc $HOME/.vimrc

#gitconfig
if [ -f ~/.gitconfig ]; then
  cp -v ~/.gitconfig $BACKUP_FOLDER/.gitconfig
fi
cp -vf $DIR/.gitconfig $HOME/.gitconfig

