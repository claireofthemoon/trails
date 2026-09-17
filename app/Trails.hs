module Trails (draw) where

import GHC.Wasm.Prim (JSString(..), JSVal, toJSString)

-- Foreign imports

foreign import javascript unsafe "$1[$2]"
  js_get_prop :: JSVal -> JSString -> IO JSVal

foreign import javascript unsafe "$1[$2]"
  js_get_prop_number :: JSVal -> JSString -> IO Double

foreign import javascript unsafe "$1[$2] = $3"
  js_set_prop_number :: JSVal -> JSString -> Double-> IO ()

foreign import javascript unsafe "devicePixelRatio"
  js_devicePixelRatio :: IO Double

foreign import javascript unsafe "requestAnimationFrame($1)"
  js_requestAnimationFrame :: JSVal -> IO ()

foreign import javascript "wrapper"
  js_requestAnimationFrame_cb :: (Double -> IO ()) -> IO JSVal

foreign import javascript unsafe "let ctx = $1; ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height);"
  js_clear_canvas :: JSVal -> IO ()

foreign import javascript unsafe "$1.fillRect($2, $3, $4, $5)"
  js_fillRect :: JSVal -> Double -> Double -> Double -> Double -> IO ()

-- Foreign exports

foreign export javascript draw :: JSVal -> IO ()
draw :: JSVal -> IO ()
draw ctx = 
  (js_requestAnimationFrame_cb $ callback ctx initialState) >>= js_requestAnimationFrame

-- IO Code

callback :: JSVal -> State -> Double -> IO ()
callback ctx state ts = do
  let state' = update ts state
  js_clear_canvas ctx
  render ctx state'
  (js_requestAnimationFrame_cb $ callback ctx state') >>= js_requestAnimationFrame

render :: JSVal -> State -> IO ()
render ctx state = do
  rp <- getRenderingProperties ctx
  let canvasWidth = width rp
  let canvasHeight = height rp
  let cx = canvasWidth / 2
  let cy = canvasHeight / 2
  let d = min canvasWidth canvasHeight
  let r = d / 2.2
  let x = r * cos (phase state) + cx
  let y = r * sin (phase state) + cy
  let w = d / 50
  js_fillRect ctx x y w w

getRenderingProperties :: JSVal -> IO RenderingProperties
getRenderingProperties ctx = do
  canvas <- js_get_prop ctx $ toJSString "canvas"
  canvasWidth <- js_get_prop_number canvas $ toJSString "offsetWidth"
  canvasHeight <- js_get_prop_number canvas $ toJSString "offsetHeight"
  dpr <- js_devicePixelRatio
  js_set_prop_number canvas (toJSString "width") (canvasWidth * dpr)
  js_set_prop_number canvas (toJSString "height") (canvasHeight * dpr)
  pure $ RenderingProperties { width = canvasWidth * dpr, height = canvasHeight * dpr }

-- Pure code

data RenderingProperties = RenderingProperties {
  width :: Double,
  height :: Double
}

data State = State {
  prevTimestamp :: Maybe Double,
  phase :: Double
}

initialState :: State
initialState = State {
  prevTimestamp = Nothing,
  phase = 0
}

update :: Double -> State -> State
update ts state = case (prevTimestamp state) of
  Nothing -> State { prevTimestamp = Just ts, phase = 0 }
  Just pts ->
    let deltaT = ts - pts
        deltaP = (deltaT / 3000) * 2 * pi
    in State { prevTimestamp = Just ts, phase = (phase state) + deltaP }
