#!/bin/sh
set -e

source ~/.ghc-wasm/env

# Build the wasm file
wasm32-wasi-cabal --with-compiler=wasm32-wasi-ghc --with-ghc-pkg=wasm32-wasi-ghc-pkg build

# Get its location
out="$(wasm32-wasi-cabal list-bin trails)"

# Build the shim file
$(wasm32-wasi-ghc --print-libdir)/post-link.mjs -i "$out" -o trails.js

# Copy both to the Vite project
cp "$out" js/static/
cp trails.js js/static/
