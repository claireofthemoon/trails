# Trails

This is an experiment in using Haskell to drive some nice HTML Canavs animations.

It uses the 'experimental' GHC Wasm backend.

## Building

The build setup is a bit chaotic. Here's what I did (M1 Mac):

1. Run the setup described in "Getting started without nix" [here](https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta)
2. Run `./build.sh`, this uses the tools from the previous step to build a Wasm file and JS shim
3. `cd js && npm install && npm run dev`

That'll get you a webpage which demos the trails. Next up I need to set up Vite to create an asset bundle I can use on my actual site - coming soon...

## References

Building libraries with Vite: https://andrewwalpole.com/blog/use-vite-for-javascript-libraries/
