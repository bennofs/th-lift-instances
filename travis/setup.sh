#!/usr/bin/env bash

unset CC
export PATH=/opt/ghc/$GHCVER/bin:/opt/cabal/$CABALVER/bin:/opt/happy/1.19.5/bin:/opt/alex/3.1.4/bin:~/tools/bin:$PATH
if [ -f $HOME/.cabal/packages/hackage.haskell.org/00-index.tar.gz ]; then
  gunzip $HOME/.cabal/packages/hackage.haskell.org/00-index.tar.gz
fi
travis_retry cabal update  

# travis docker containers report incorrect number of cores
sed -i 's/^jobs:.*$/jobs: 2/' $HOME/.cabal/config
