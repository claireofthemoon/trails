module Trails (draw) where

import Data.Fixed (mod')
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
  let radius = canvasWidth / 12
  let (cx, cy) = centreToPixels radius (centre $ trailState state)
  let d = min canvasWidth canvasHeight
  let x = radius * cos (phase (trailState state)) + cx
  let y = radius * sin (phase (trailState state)) + cy
  let w = d / 100
  js_fillRect ctx x y w w

-- Even rows are unshifted
-- Odd rows are shifted right by one radius
centreToPixels :: Double -> Centre -> (Double, Double)
centreToPixels radius (Centre (cx, cy)) =
  let x = fromIntegral cx * radius * 2
      y = fromIntegral cy * radius * sqrt 3.0
      dx = if even cy then 0 else radius
  in (x + dx, y)
  

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
  trailState :: TrailState
}

initialState :: State
initialState = State {
  prevTimestamp = Nothing,
  trailState = TrailState {
    centre = Centre (2, 1),
    spin  = Clockwise,
    phase = 0
  }
}

data TrailState = TrailState {
  centre :: Centre,
  spin :: Spin,
  phase :: Double
} deriving (Show)

update :: Double -> State -> State
update ts state = case (prevTimestamp state) of
  Nothing -> state { prevTimestamp = Just ts  }
  Just pts ->
    let deltaT = ts - pts
        trail = trailState state
    in State {
      prevTimestamp = Just ts,
      trailState = updateTrailState deltaT trail
    }

speed :: Double
speed = 2 * pi / 3000

data Segment
  = Seg1
  | Seg2
  | Seg3
  | Seg4
  | Seg5
  | Seg6
    deriving (Eq, Show)

newtype Centre = Centre (Int, Int) deriving (Show)

data Spin = Clockwise | Counterclockwise deriving (Show)

rev :: Spin -> Spin
rev Clockwise = Counterclockwise
rev Counterclockwise = Clockwise

spinSign :: Spin -> Double
spinSign Clockwise = 1
spinSign Counterclockwise = -1

segment :: Double -> Segment
segment p =
  let segmentSize = 2 * pi / 6
  in if      p < 1 * segmentSize then Seg1
     else if p < 2 * segmentSize then Seg2
     else if p < 3 * segmentSize then Seg3
     else if p < 4 * segmentSize then Seg4
     else if p < 5 * segmentSize then Seg5
     else                             Seg6

updateTrailState :: Double -> TrailState -> TrailState
updateTrailState dt state =
  let prevPhase = phase state
      nextPhase = wrapPhase $ prevPhase + dt * speed * (spinSign $ spin state)
      prevSegment = segment prevPhase
      nextSegment = segment nextPhase
      -- Using prevPhase might look odd here, but it means the phase flips
      -- to the correct side of the next segment boundary.
      (c, s, p) = jump prevPhase nextSegment (spin state) (centre state)
  in if nextSegment /= prevSegment
     then state { centre = c, spin = s, phase = p }
     else state { phase = nextPhase }

wrapPhase :: Double -> Double
wrapPhase p = p `mod'` (2 * pi)

jump :: Double -> Segment -> Spin -> Centre -> (Centre, Spin, Double)
jump p seg spn c =
  let jumpDir = adjacent seg spn
      next = move jumpDir c
  in (next, rev spn, wrapPhase $ p + pi) -- this flips to wrong side of seg bdry

data Direction
  = DRight
  | DDownRight
  | DDownLeft
  | DLeft
  | DUpLeft
  | DUpRight

-- If we've just crossed a segment border in a given spin, which direction is adjacent?
adjacent :: Segment -> Spin -> Direction
adjacent seg spn =
  case (seg, spn) of
    (Seg1, Clockwise) -> DRight
    (Seg2, Clockwise) -> DDownRight
    (Seg3, Clockwise) -> DDownLeft
    (Seg4, Clockwise) -> DLeft
    (Seg5, Clockwise) -> DUpLeft
    (Seg6, Clockwise) -> DUpRight
    (Seg1, Counterclockwise) -> DDownRight
    (Seg2, Counterclockwise) -> DDownLeft
    (Seg3, Counterclockwise) -> DLeft
    (Seg4, Counterclockwise) -> DUpLeft
    (Seg5, Counterclockwise) -> DUpRight
    (Seg6, Counterclockwise) -> DRight

-- Which circle index is adjacent in the given direction?
-- Even rows have an extra circle, which adjusts the indexing.
-- In an even row, up/down right is +0, left is -1
-- In an odd row, up/down right is +1, left is +0
move :: Direction -> Centre -> Centre
move dir (Centre (cx, cy)) =
  let adjustment = if even cy then -1 else 0
  in Centre $ case dir of
    DRight -> (cx + 1, cy)
    DDownRight -> (cx + 1 + adjustment, cy + 1)
    DDownLeft -> (cx + adjustment, cy + 1)
    DLeft -> (cx - 1, cy)
    DUpLeft -> (cx + adjustment, cy - 1)
    DUpRight -> (cx + 1 + adjustment, cy - 1)

{-
- How does the circle layout work?
-
- We fit six full circles into the width.
- The first row starts and ends at circle centres (so quarter circles).
- We then tile that down in a hexagonal pattern, sqrt(3) * radius step.
-
- How can we index these in a way that represents connectivity?
-
- Coordinate systems:
- For circle layout, graphics (top left is 0,0 and positive is right/down)
- Within a circle, 0 phase is (1, 0) and clockwise is positive
-}
