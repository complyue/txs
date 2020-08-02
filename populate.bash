#!/usr/bin/env bash

cd "$(dirname "$0")"
mkdir build 2>/dev/null
cd build

ghc --make -Wall -threaded -rtsopts -prof -o txs -i../src ../src/Main.hs && (

  ./txs +RTS -N6 -A32m -H256m -qg -I0 -M2g -T -s < ../scripts/populate.txs

)
