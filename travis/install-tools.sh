#!/usr/bin/env bash

set -e

ls -l $HOME/tools/.cabal-sandbox

if [ ! -d $HOME/tools/bin ]; then
    mkdir -p $HOME/tools
    cd $HOME/tools
    cabal-$CABALVER sandbox init
    cabal-$CABALVER install hlint packunused -j
    ln -s $PWD/.cabal-sandbox/bin $PWD/bin
    cd $TRAVIS_BUILD_DIR
fi
