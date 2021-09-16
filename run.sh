#!/usr/bin/env bash

runhaskell Setup.hs configure
runhaskell Setup.hs build
dist/build/cells/cells
