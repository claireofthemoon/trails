#!/bin/sh
source ~/.ghc-wasm/env
wasm32-wasi-cabal --with-compiler=wasm32-wasi-ghc --with-ghc-pkg=wasm32-wasi-ghc-pkg build
