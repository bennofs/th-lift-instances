#!/usr/bin/env bash

set -e

begin_steps

# We will first compute cabal's install plan. If it matches the install plan in the cache,
# we can reuse the cache. Otherwise, we will throw away the cache to avoid interfering with
# cabal's solver.
step "Computing install plan" << EOF
  cabal install --dry -v --only-dependencies --enable-tests --enable-benchmarks ${ALLOW_NEWER:+--allow-newer="$ALLOW_NEWER"} > installplan.txt

  # -v prints things some things we are not interested in, like commands containing random temporary path.
  # The install plan starts after the line "Resolving dependencies...", so we will just delete everything up to that.
  sed -i -e '1,/^Resolving dependencies/d' installplan.txt

  # Print out the install plan for debugging purposes
  cat installplan.txt
EOF

if diff -u installplan.txt $HOME/.cabsnap/installplan.txt; then
  step "Restoring build cache" << EOF
    rm -rf .ghc
    cp -a $HOME/.cabsnap/ghc $HOME/.ghc
    cp -a $HOME/.cabsnap/lib $HOME/.cabsnap/share $HOME/.cabsnap/bin $HOME/.cabal
EOF
else
  step "Clearing build cache and installing dependencies" << EOF
    rm -rf $HOME/.cabsnap
    mkdir -p $HOME/.ghc $HOME/.cabal/lib $HOME/.cabal/share $HOME/.cabal/bin
    cabal install --only-dependencies --enable-tests --enable-benchmarks ${ALLOW_NEWER:+--allow-newer="$ALLOW_NEWER"}
    if [ "$GHCVER" = "7.10.1" ]; then cabal install Cabal-1.22.4.0; fi
EOF
  step "Saving build cache" << EOF
    mkdir $HOME/.cabsnap
    cp -a $HOME/.ghc $HOME/.cabsnap/ghc
    cp -a $HOME/.cabal/lib $HOME/.cabal/share $HOME/.cabal/bin installplan.txt $HOME/.cabsnap/
EOF
fi

if [ ! -z $ROOT ]; then
  step "Computing tool versions" << EOF
    cabal list --simple-output hlint | tail -n1 > toolversions.txt
    cabal list --simple-output packunused | tail -n1 >> toolversions.txt
    cat toolversions.txt
EOF
  if ! diff -u toolversions.txt $HOME/tools/toolversions.txt; then
    step "Installing tools" << EOF
      rm -rf $HOME/tools
      mkdir -p $HOME/tools
      cd $HOME/tools
      cabal sandbox init
      cabal install hlint packunused
      ln -s $HOME/tools/.cabal-sandbox/bin $HOME/tools/bin
EOF
    cp toolversions.txt $HOME/tools
  fi
fi

end_steps
