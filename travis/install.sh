#!/usr/bin/env bash

set -e

begin_steps

# We will first compute cabal's install plan. If it matches the install plan in the cache,
# we can reuse the cache. Otherwise, we will throw away the cache to avoid interfering with
# cabal's solver.
CABAL_FULL_VERSION=$(cabal --version | grep -P -o 'using version \K[^ ]+')
step "Computing install plan" << EOF
  if [ -n "$V2" ]; then
    cabal v2-build --dry -v --only-dependencies --enable-tests --enable-benchmarks ${ALLOW_NEWER:+--allow-newer="$ALLOW_NEWER"} > installplan.txt
    sed -i -e '1,/^In order, the following would be built:/d' installplan.txt
  else
    cabal install --dry -v --only-dependencies --enable-tests --enable-benchmarks ${ALLOW_NEWER:+--allow-newer="$ALLOW_NEWER"} > installplan.txt
    # -v prints things some things we are not interested in, like commands containing random temporary path.
    # The install plan starts after the line "Resolving dependencies...", so we will just delete everything up to that.
    sed -i -e '1,/^Resolving dependencies/d' installplan.txt
  fi

  # We need to install a Cabal library that matches the version cabal-install was compiled against
  echo "Cabal $CABAL_FULL_VERSION" >> installplan.txt

  # Print out the install plan for debugging purposes
  cat installplan.txt
EOF

if diff -u installplan.txt $HOME/.cabsnap/installplan.txt; then
  step "Restoring build cache" << EOF
    rm -rf .ghc
    cp -a $HOME/.cabsnap/ghc $HOME/.ghc
    mkdir -p $HOME/.cabsnap/lib $HOME/.cabsnap/share $HOME/.cabsnap/bin $HOME/.cabsnap/store
    cp -a $HOME/.cabsnap/lib $HOME/.cabsnap/share $HOME/.cabsnap/bin $HOME/.cabsnap/store $HOME/.cabal
EOF
else
  step "Clearing build cache and installing dependencies" << EOF
    rm -rf $HOME/.cabsnap
    mkdir -p $HOME/.ghc $HOME/.cabal/lib $HOME/.cabal/share $HOME/.cabal/bin $HOME/.cabal/store
    if [ -n "$V2" ]; then
      cabal v2-build --only-dependencies --enable-tests --enable-benchmarks ${ALLOW_NEWER:+--allow-newer="$ALLOW_NEWER"}
    else
      cabal install --only-dependencies --enable-tests --enable-benchmarks ${ALLOW_NEWER:+--allow-newer="$ALLOW_NEWER"}
      cabal install Cabal --constraint "Cabal==$CABAL_FULL_VERSION"
    fi
EOF
  step "Saving build cache" << EOF
    mkdir $HOME/.cabsnap
    cp -a $HOME/.ghc $HOME/.cabsnap/ghc
    cp -a $HOME/.cabal/lib $HOME/.cabal/share $HOME/.cabal/bin $HOME/.cabal/store installplan.txt $HOME/.cabsnap/
EOF
fi

if [ ! -z $ROOT ]; then
  TOOLS="hlint packunused haddock"

  step "Computing tool versions" << EOF
    echo "Cabal $CABAL_FULL_VERSION" > toolversions.txt
    for tool in $TOOLS; do
      cabal list --simple-output \$tool | grep "^\$tool " | tail -n1 >> toolversions.txt
    done
    cat toolversions.txt
EOF
  if ! diff -u toolversions.txt $HOME/tools/toolversions.txt; then
    step "Installing tools" << EOF
      rm -rf $HOME/tools
      mkdir -p $HOME/tools
      cabal --store-dir $HOME/tools v2-install --installdir $HOME/tools/bin $TOOLS --constraint "Cabal==$CABAL_FULL_VERSION" --allow-newer
      rm -f $HOME/tools/bin/{happy,ghc,alex,cabal}
EOF
    cp toolversions.txt $HOME/tools
  fi
fi

end_steps
