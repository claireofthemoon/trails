#!/bin/sh
source ~/.ghc-wasm/env
cabal --with-compiler=wasm32-wasi-ghc --with-ghc-pkg=wasm32-wasi-ghc-pkg build
