module Trails (draw) where

import Control.Monad.State.Strict (MonadState, runState, state)
import Data.Fixed (mod')
import GHC.Wasm.Prim (JSString(..), JSVal, toJSString)
import System.Random (RandomGen, getStdGen, random)

import qualified Data.Set as Set

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
draw ctx = do
  g <- getStdGen
  (js_requestAnimationFrame_cb $ callback g ctx initialState) >>= js_requestAnimationFrame

-- IO Code

callback :: RandomGen g => g -> JSVal -> AnimationState -> Double -> IO ()
callback g ctx s ts = do
  let (s', g') = runState (update ts s) g
  js_clear_canvas ctx
  render ctx s'
  (js_requestAnimationFrame_cb $ callback g' ctx s') >>= js_requestAnimationFrame

render :: JSVal -> AnimationState -> IO ()
render ctx s = do
  rp <- getRenderingProperties ctx
  let canvasWidth = width rp
  let canvasHeight = height rp
  let radius = canvasWidth / 12
  let (cx, cy) = centreToPixels radius (centre $ trailState s)
  let d = min canvasWidth canvasHeight
  let x = radius * cos (phase (trailState s)) + cx
  let y = radius * sin (phase (trailState s)) + cy
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

data AnimationState = AnimationState {
  prevTimestamp :: Maybe Double,
  trailState :: TrailState
}

initialState :: AnimationState
initialState = AnimationState {
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

update :: (RandomGen g, MonadState g m) => Double -> AnimationState -> m AnimationState
update ts s = case (prevTimestamp s) of
  Nothing -> pure $ s { prevTimestamp = Just ts  }
  Just pts -> do
    let deltaT = ts - pts
    let trail = trailState s
    trs <- updateTrailState deltaT trail
    pure $ AnimationState {
      prevTimestamp = Just ts,
      trailState = trs
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

data Spin = Clockwise | Counterclockwise deriving (Eq, Show)

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

updateTrailState :: (RandomGen g, MonadState g m) => Double -> TrailState -> m TrailState
updateTrailState dt s =
  let prevPhase = phase s
      nextPhase = wrapPhase $ prevPhase + dt * speed * (spinSign $ spin s)
      prevSegment = segment prevPhase
      nextSegment = segment nextPhase
     -- Using prevPhase might look odd here, but it means the phase flips
     -- to the correct side of the next segment boundary.
      (dir, c, spn, p) = jump prevPhase nextSegment (spin s) (centre s)
  in if nextSegment /= prevSegment
     then if mustMove dir (centre s)
            then pure $ s { centre = c, spin = spn, phase = p }
            else if canMove dir (centre s) (spin s)
              then do
                shouldMove <- coinFlip
                pure $ if shouldMove
                  then s { centre = c, spin = spn, phase = p }
                  else s { phase = nextPhase }
              else pure $ s { phase = nextPhase }
     else pure $ s { phase = nextPhase }


wrapPhase :: Double -> Double
wrapPhase p = p `mod'` (2 * pi)

jump :: Double -> Segment -> Spin -> Centre -> (Direction, Centre, Spin, Double)
jump p seg spn c =
  let jumpDir = adjacent seg spn
      next = move jumpDir c
  in (jumpDir, next, rev spn, wrapPhase $ p + pi)

coinFlip :: (RandomGen g, MonadState g m) => m Bool
coinFlip = state random

data Direction
  = DRight
  | DDownRight
  | DDownLeft
  | DLeft
  | DUpLeft
  | DUpRight
    deriving (Eq, Ord)

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

isTop :: Centre -> Bool
isTop (Centre (_, cy)) = cy == 0

isSecondTop :: Centre -> Bool
isSecondTop (Centre (_, cy)) = cy == 1

isLeftEven :: Centre -> Bool
isLeftEven (Centre (cx, cy)) = cx == 0 && even cy

isLeftOdd :: Centre -> Bool
isLeftOdd (Centre (cx, cy)) = cx == 0 && odd cy

isRightEven :: Centre -> Bool
isRightEven (Centre (cx, cy)) = cx == 6 && even cy

isRightOdd :: Centre -> Bool
isRightOdd (Centre (cx, cy)) = cx == 5 && odd cy

-- TODO: fix to use actual bounds

isBottom :: Centre -> Bool
isBottom (Centre (_, cy)) = cy == 4

isSecondBottom :: Centre -> Bool
isSecondBottom (Centre (_, cy)) = cy == 3

mustMove :: Direction -> Centre -> Bool
mustMove dir c =
  let -- All accessible moves from the top edge are mandatory
      top    = isTop c
      -- In an even row on the left edge, UpRight and DownRight are mandatory
      left   = isLeftEven c && (dir == DUpRight || dir == DDownRight)
      -- In an even row on the right edge, UpLeft and DownLeft are mandatory
      right  = isRightEven c && (dir == DUpLeft || dir == DDownLeft)
      bottom = isBottom c
  in top || left || right || bottom

canMove :: Direction -> Centre -> Spin -> Bool
canMove dir c spn =
  let allDirs = Set.fromList [DRight, DDownRight, DDownLeft, DLeft, DUpLeft, DUpRight]
      top = Set.fromList [DDownLeft, DDownRight]
      secondTop = Set.fromList $ if spn == Clockwise
        then [DUpRight, DRight, DDownRight, DDownLeft, DLeft]
        else [DRight, DDownRight, DDownLeft, DLeft, DUpLeft]
      leftEven = Set.fromList [DUpRight, DDownRight, DRight]
      leftOdd = Set.fromList $ if spn == Clockwise
          then [DUpLeft, DUpRight, DRight, DDownRight]
          else [DUpRight, DRight, DDownRight, DDownLeft]
      rightEven = Set.fromList [DUpLeft, DDownLeft, DLeft]
      rightOdd = Set.fromList $ if spn == Clockwise
          then [DDownRight, DDownLeft, DLeft, DUpLeft]
          else [DDownLeft, DLeft, DUpLeft, DUpRight]
      bottom = Set.fromList [DUpLeft, DUpRight]
      secondBottom = Set.fromList $ if spn == Clockwise
        then [DDownLeft, DLeft, DUpLeft, DUpRight, DRight]
        else [DLeft, DUpLeft, DUpRight, DRight, DDownRight]

      topCandidates = if isTop c then top else allDirs
      secondTopCandidates = if isSecondTop c then secondTop else allDirs
      leftEvenCandidates = if isLeftEven c then leftEven else allDirs
      leftOddCandidates = if isLeftOdd c then leftOdd else allDirs
      rightEvenCandidates = if isRightEven c then rightEven else allDirs
      rightOddCandidates = if isRightOdd c then rightOdd else allDirs
      bottomCandidates = if isBottom c then bottom else allDirs
      secondBottomCandidates = if isSecondBottom c then secondBottom else allDirs
      allowed = foldr Set.intersection allDirs [
          topCandidates,
          secondTopCandidates,
          leftEvenCandidates,
          leftOddCandidates,
          rightEvenCandidates,
          rightOddCandidates,
          bottomCandidates,
          secondBottomCandidates
        ]
  in Set.member dir allowed

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
-
- Boundaries: each provides a constraint, which stacks.
- Left edge, even row: UpRight or DownRight _must_ be taken. Right is optional. Everything else is banned.
- Left edge, odd row: UpLeft can only be taken if going clockwise, DownLeft can only be taken if going counterclockwise. Left is banned.
- Right edge, even row: UpLeft or DownLeft _must_ be taken. Left is optional. Everything else is banned.
- Right edge, odd row: UpRight can only be taken if going counterclockwise. DownRight can only be taken if going clockwise. Right is banned.
- Top edge: DownLeft and DownRight _must_ be taken.
- Second from top edge: UpLeft can only be taken if going counterclockwise. UpRight can only be taken if going clockwise.
- Bottom edge: UpLeft and UpRight _must_ be taken.
- Second from bottom edge: DownRight can only be taken if going counterclockwise. DownLeft can only be taken if going clockwise.

- Two kinds of things here: mandatory actions and optional one. Check mandatory first, then check if desired action is in the allowed optional set.

-}
