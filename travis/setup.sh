#!/usr/bin/env bash

set -e

green='\e[0;32m'
red='\e[0;31m'
nc='\e[0m' # No Color

function step {
  echo -e "${green}$1 ...${nc}"
  bash /dev/stdin || exit 1
}

function step_suppress {
  echo -ne "${green}$1 ... ${nc}"
  tmp=$(mktemp)
  bash /dev/stdin &> $tmp && echo -e "${green}Done${nc}" || (
    echo -e "${red}Failed${nc}"
    echo "Output: "
    cat $tmp
    exit 1
  )
}

export -f step step_supress

unset CC
export PATH=/opt/ghc/$GHCVER/bin:/opt/cabal/$CABALVER/bin:/opt/happy/1.19.5/bin:/opt/alex/3.1.4/bin:~/tools/bin:$PATH
if [ -f $HOME/.cabal/packages/hackage.haskell.org/00-index.tar.gz ]; then
  gunzip $HOME/.cabal/packages/hackage.haskell.org/00-index.tar.gz
fi
travis_retry cabal update  

# travis docker containers report incorrect number of cores
sed -i 's/^jobs:.*$/jobs: 2/' $HOME/.cabal/config
