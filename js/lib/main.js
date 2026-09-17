import { WASI } from "@bjorn3/browser_wasi_shim";

// Use Vite's url syntax to get the (hashed) asset path
import trailsWasm from '/static/trails.wasm?url'
import trailsJs from '/static/trails.js'

let __exports = {};

let wasi = new WASI([], [], []);
let importObject = {
  ghc_wasm_jsffi: trailsJs(__exports),
  wasi_snapshot_preview1: wasi.wasiImport,
};

let { instance } = await WebAssembly.instantiateStreaming(
  fetch(trailsWasm),
  importObject
);

Object.assign(__exports, instance.exports);
wasi.initialize(instance);


const { hs_init, draw } = __exports;
hs_init();
console.log(draw({ height: 300 }))

