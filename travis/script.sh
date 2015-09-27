#!/usr/bin/env bash

step "Configuring project" << 'EOF'
  cabal configure --enable-tests --enable-benchmarks -v2 --ghc-options="-Wall -Werror -ddump-minimal-imports" &> cabal.log || (
    cat cabal.log 
    exit 1
  )
  echo "Using packages: "
  sed -nre 's/Dependency ([^ ]*) ==([^ :]*).*/\1 \2/p' cabal.log | column -t | sed -e "s/^/  /"
  echo "Flags chosen: "
  sed -nr -e ':x; /\,$/ { N; s/,\n/,/; tx }' -e 's/Flags chosen: (.*)/\1/' -e 's/, /,/gp' cabal.log | tr ',' '\n'
EOF

step "Building project" << EOF
  cabal build
EOF

step "Running tests" << EOF
  cabal test
EOF

step "Creating source distribution" << EOF
  cabal check
  cabal sdist # tests that a source-distribution can be generated
EOF

step_suppress "Checking source distribution" << 'EOF'
  # The following scriptlet checks that the resulting source distribution can be built & installed
  SRC_TGZ=$(cabal info . | awk '{print $2 ".tar.gz";exit}')
  cd dist/
  if [ -f "$SRC_TGZ" ]; then
    cabal install --enable-tests --enable-benchmarks "$SRC_TGZ"
  else
    echo "expected '$SRC_TGZ' not found"
    exit 1
  fi    
EOF

[ -z "$ROOT" ] && exit 0 || true

step "Checking style with HLint" << EOF
  hlint --cpp-simple src tests
EOF

step_suppress "Checking for unused dependencies" << EOF
  packunused
EOF
