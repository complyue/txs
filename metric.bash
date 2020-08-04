#!/usr/bin/env bash

SCRIPT=${1:-populate}

cd "$(dirname "$0")"
mkdir build 2>/dev/null
cd build

ghc --make -Wall -threaded -rtsopts -prof -o txs -outputdir . -stubdir . -i../src ../src/Main.hs && (

  ./txs +RTS -N10 -A32m -H256m -qg -I0 -M5g -T -s <../scripts/"${SCRIPT}".txs

)
