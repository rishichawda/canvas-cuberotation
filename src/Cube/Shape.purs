module Cube.Shape where

import Data.Array ((!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Graphics.Canvas (CANVAS, Context2D, beginPath, clearRect, closePath, getCanvasElementById, getCanvasHeight, getCanvasWidth, getContext2D, lineTo, moveTo, setCanvasHeight, setCanvasWidth, setFillStyle, setStrokeStyle, stroke, translate, withContext)
import Prelude (Unit, bind, discard, negate, pure, void, ($), (*), (+), (-), (/))
import Control.Monad.Eff (Eff, forE)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF, Ref, newRef, readRef, writeRef)
import Control.Monad.ST (ST)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (Window)
import DOM.HTML.Window (requestAnimationFrame)
import Data.Array.ST (STArray, emptySTArray, peekSTArray, pushSTArray)
import Math (cos, pi, sin)
import Partial.Unsafe (unsafePartial)

-- Rotation on z axis
rotate_z :: forall t25 t8.
  STArray t25 (Array Number)
  -> Number
     -> Eff
          ( st :: ST t25
          | t8
          )
          (STArray t25 (Array Number))
rotate_z nodes angle = do
  let sint = sin angle
  let cost = cos angle
  newnodes <- emptySTArray
  void $ forE 0 8 $ \i -> do
    jnode <- peekSTArray nodes i
    let node = fromMaybe [] jnode
    let jx = node !! 0
    let jy = node !! 1
    let jz = node !! 2
    let x = fromMaybe 0.0 jx
    let y = fromMaybe 0.0 jy
    let z = fromMaybe 0.0 jz
    let nx = x * cost - y * sint
    let ny = y * cost + x * sint
    let nn = [ nx, ny, z ]
    void $ pushSTArray newnodes nn
  pure newnodes

-- Rotation on y axis
rotate_y :: forall t66 t83.
  STArray t83 (Array Number)
  -> Number
     -> Eff
          ( st :: ST t83
          | t66
          )
          (STArray t83 (Array Number))
rotate_y nodes angle = do
  let sint = sin angle
  let cost = cos angle
  newnodes <- emptySTArray
  void $ forE 0 8 $ \i -> do
    jnode <- peekSTArray nodes i
    let node = fromMaybe [] jnode
    let jx = node !! 0
    let jy = node !! 1
    let jz = node !! 2
    let x = fromMaybe 0.0 jx
    let y = fromMaybe 0.0 jy
    let z = fromMaybe 0.0 jz
    let nx = x * cost - z * sint
    let nz = z * cost + x * sint
    let nn = [ nx, y, nz ]
    void $ pushSTArray newnodes nn
  pure newnodes

-- Rotation on x axis
rotate_x :: forall t124 t141.
  STArray t141 (Array Number)
  -> Number
     -> Eff
          ( st :: ST t141
          | t124
          )
          (STArray t141 (Array Number))
rotate_x nodes angle = do
  let sint = sin angle
  let cost = cos angle
  newnodes <- emptySTArray
  void $ forE 0 8 $ \i -> do
    jnode <- peekSTArray nodes i
    let node = fromMaybe [] jnode
    let jx = node !! 0
    let jy = node !! 1
    let jz = node !! 2
    let x = fromMaybe 0.0 jx
    let y = fromMaybe 0.0 jy
    let z = fromMaybe 0.0 jz
    let ny = y * cost - z * sint
    let nz = z * cost + y * sint
    let nn = [ x, ny, nz ]
    void $ pushSTArray newnodes nn
  pure newnodes

-- Rotation function on the cube
rotate_cube :: forall t309 t310 t312.
  Context2D
  -> { n :: STArray t310 (Array Number)
     , x_theta :: Number
     , y_theta :: Number
     , z_theta :: Number
     , e :: STArray t310 (Array Int)
     | t312
     }
     -> Eff
          ( st :: ST t310
          , canvas :: CANVAS
          | t309
          )
          (STArray t310 (Array Number))
rotate_cube context state = do
  nnz <- rotate_z state.n state.x_theta
  nny <- rotate_y nnz state.y_theta
  nnx <- rotate_x nny state.z_theta
  draw_faces nnx state.e context
  pure nnx

-- Draw a line between two points.
draw_line :: forall t183.
  Number
  -> Number
     -> Number
        -> Number
           -> Context2D
              -> Eff
                   ( canvas :: CANVAS
                   | t183
                   )
                   Unit
draw_line mxv myv lxv lyv context = do
  _ <- moveTo context mxv myv
  void $ lineTo context lxv lyv

-- Draw the faces of cube
draw_faces :: forall t205 t269.
  STArray t205 (Array Number)
  -> STArray t205 (Array Int)
     -> Context2D
        -> Eff
             ( canvas :: CANVAS
             , st :: ST t205
             | t269
             )
             Unit
draw_faces nodes edges context = do
  void $ withContext context do
    _ <- setStrokeStyle "#0000FF" context
    _ <- beginPath context
    jn <- emptySTArray
    void $ forE 0 12 $ \i -> do
      jn <- peekSTArray edges i
      let n = fromMaybe [] jn
      let jnva = n !! 0
      let nva = fromMaybe 0 jnva
      let jnvb = n !! 1
      let nvb = fromMaybe 0 jnvb
      jna <- peekSTArray nodes nva
      let na = fromMaybe [] jna
      jnb <- peekSTArray nodes nvb
      let nb = fromMaybe [] jnb
      let jmxv = na !! 0
      let mxv = fromMaybe 0.0 jmxv
      let jmyv = na !! 1
      let myv = fromMaybe 0.0 jmyv
      let jlxv = nb !! 0
      let lxv = fromMaybe 0.0 jlxv
      let jlyv = nb !! 1
      let lyv = fromMaybe 0.0 jlyv
      draw_line mxv myv lxv lyv context
      -- log $ show mxv <> " , " <> show myv <> " : " <> show lxv <> " , " <> show lyv
    _ <- closePath context
    stroke context

-- Draw the cube.
draw_cube :: forall t331 t332 t361.
  Partial => Context2D
             -> { n :: STArray t332 (Array Number)
                , x_theta :: Number
                , y_theta :: Number
                , z_theta :: Number
                , e :: STArray t332 (Array Int)
                | t331
                }
                -> Eff
                     ( console :: CONSOLE
                     , canvas :: CANVAS
                     , st :: ST t332
                     | t361
                     )
                     Unit
draw_cube context state = void $ do
  Just element <- getCanvasElementById "canvas"
  width <- getCanvasWidth element
  height <- getCanvasHeight element
  context <- setFillStyle "#F8F9FA" context
  nnx <- rotate_cube context state
  nnx <- rotate_cube context state
  nnx <- rotate_cube context state
  nnx <- rotate_cube context state
  _ <- clearRect context {x: -200.0, y: -200.0, w: width, h: height}
  rotate_cube context state

-- Animation function for rotation.
animation :: forall t377 t401 t402.
  Window
  -> Ref t401
     -> t377
        -> (t401
            -> Eff
                 ( ref :: REF
                 , dom :: DOM
                 | t402
                 )
                 t401
           )
           -> Eff
                ( dom :: DOM
                , ref :: REF
                | t402
                )
                Unit
animation window ref state step =
  void $ requestAnimationFrame
    do animation window ref state step
       state <- readRef ref
       state <- step state
       writeRef ref state
    window

-- Start animation / rotation
animate :: forall t427 t431.
  Partial => { size :: Number
             | t427
             }
             -> (Context2D
                 -> { size :: Number
                    | t427
                    }
                    -> Eff
                         ( canvas :: CANVAS
                         , dom :: DOM
                         , ref :: REF
                         | t431
                         )
                         { size :: Number
                         | t427
                         }
                )
                -> Eff
                     ( dom :: DOM
                     , canvas :: CANVAS
                     , ref :: REF
                     | t431
                     )
                     Unit
animate state update_canv = do
  window <- window
  ref <- newRef state
  animation window ref state \state -> do
    Just element <- getCanvasElementById "canvas"
    context <- getContext2D element
    _ <- clearRect context {x: -200.0, y: -200.0, w: state.size, h: state.size}
    update_canv context state

-- Update the canvas
update_canvas :: forall t370 t371 t373.
  Partial => { size :: Number
             , e :: STArray t371 (Array Int)
             , n :: STArray t371 (Array Number)
             , x_theta :: Number
             , y_theta :: Number
             , z_theta :: Number
             | t373
             }
             -> Context2D
                -> Eff
                     ( canvas :: CANVAS
                     , console :: CONSOLE
                     , st :: ST t371
                     | t370
                     )
                     Unit
update_canvas state context = do
  _ <- clearRect context {x: -200.0, y: -200.0, w: state.size, h: state.size}
  draw_cube context state

-- Animate the cube.
animate_cube :: forall t424 t431 t432 t436.
  Partial => { size :: Number
             , e :: STArray t432 (Array Int)
             , n :: STArray t432 (Array Number)
             , x_theta :: Number
             , y_theta :: Number
             , z_theta :: Number
             | t431
             }
             -> t424
                -> ({ size :: Number
                    , e :: STArray t432 (Array Int)
                    , n :: STArray t432 (Array Number)
                    , x_theta :: Number
                    , y_theta :: Number
                    , z_theta :: Number
                    | t431
                    }
                    -> Context2D
                       -> Eff
                            ( canvas :: CANVAS
                            , console :: CONSOLE
                            , st :: ST t432
                            , dom :: DOM
                            , ref :: REF
                            | t436
                            )
                            { size :: Number
                            , e :: STArray t432 (Array Int)
                            , n :: STArray t432 (Array Number)
                            , x_theta :: Number
                            , y_theta :: Number
                            , z_theta :: Number
                            | t431
                            }
                   )
                   -> Eff
                        ( dom :: DOM
                        , canvas :: CANVAS
                        , ref :: REF
                        , console :: CONSOLE
                        , st :: ST t432
                        | t436
                        )
                        Unit
animate_cube state context draw = do
  animate state \state context -> do
    update_canvas context state
    draw context state


main :: forall t613 t625.
  Eff
    ( canvas :: CANVAS
    , console :: CONSOLE
    , dom :: DOM
    , ref :: REF
    , st :: ST t625
    | t613
    )
    Unit
main = void $ unsafePartial do
  Just element <- getCanvasElementById "canvas"
  context <- getContext2D element
  _ <- setCanvasHeight 520.0 element
  _ <- setCanvasWidth 520.0 element
  _ <- translate { translateX: 200.0, translateY:  200.0 } context
  let nodes_array = [ [-100.0, -100.0, -100.0], [-100.0, -100.0, 100.0], [-100.0, 100.0, -100.0], [-100.0, 100.0, 100.0], [100.0, -100.0, -100.0], [100.0, -100.0, 100.0], [100.0, 100.0, -100.0], [100.0, 100.0, 100.0] ]
  let edges_array = [ [0, 1], [1, 3], [3, 2], [2, 0], [4, 5], [5, 7], [7, 6], [6, 4], [0, 4], [1, 5], [2, 6], [3, 7] ]
  nodes <- emptySTArray

  void $ forE 0 8 $ \n -> do
    let xx = nodes_array !! n
    let yy = fromMaybe [] xx
    void $ pushSTArray nodes yy

  edges <- emptySTArray
  void $ forE 0 12 $ \e -> do
    let je = edges_array !! e
    let e = fromMaybe [] je
    void $ pushSTArray edges e

  let state = { n: nodes, e: edges, x_theta: pi / 4.0, y_theta: pi / 3.0, z_theta: pi / 4.0, size: 520.0 }
  
  void $ animate_cube state context \state context -> do
    void $ rotate_cube context state
    pure $ state { n = nodes, e = edges, x_theta = state.x_theta + 0.043, y_theta = state.y_theta + 0.043, z_theta = state.z_theta + 0.043, size = state.size }