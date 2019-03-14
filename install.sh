#!/bin/bash
mkdir build
stack setup
stack build --copy-bins --local-bin-path build  --ghc-options -O2
rm -r .stack-work
