# Trails

This is an experiment in using Haskell to drive some nice HTML Canavs animations.

It uses the 'experimental' GHC Wasm backend.

## Building

The build setup is a bit chaotic. Here's what I did (M1 Mac):

1. Run the setup described in "Getting started without nix" [here](https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta)
2. Run `./build.sh`, this uses the tools from the previous step to build a Wasm file and JS shim
3. `cd js && npm install && npm run dev`

That'll get you a webpage which demos the trails.

If you run `npm run build`, everything gets bundled into a single ESM file that you can just import - see `index.html`.

## References

Building libraries with Vite: https://andrewwalpole.com/blog/use-vite-for-javascript-libraries/

GHC Wasm backend docs: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html

GHC Wasm primitives: https://github.com/ghc/ghc/blob/master/libraries/ghc-internal/src/GHC/Internal/Wasm/Prim.hs

