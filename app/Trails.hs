module Trails (draw) where

import GHC.Wasm.Prim (JSString(..), JSVal, toJSString)

-- Foreign imports

foreign import javascript unsafe "$1[$2]" js_prop :: JSVal -> JSString -> IO JSVal

foreign import javascript safe "new Promise(() => console.log($1))" js_print :: JSVal -> IO ()

-- Foreign exports

foreign export javascript draw :: JSVal -> IO ()
draw :: JSVal -> IO ()
draw canvas = do
  height <- js_prop canvas (toJSString "height")
  js_print height
