#!/usr/bin/env bash

cd "$(dirname "$0")"
mkdir build 2>/dev/null
cd build

ghc --make -Wall -threaded -rtsopts -prof -o txs -outputdir . -stubdir . -i../src ../src/Main.hs && (

  echo ' assert 23 $ 8 + 3 * (7-2) ' | ./txs

)
