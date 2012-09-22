{-# LANGUAGE FlexibleInstances #-}
module Animate where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Control.Monad
import Data.IORef

class Draw a where
  draw :: a -> IO ()
  
class Animate a where
  animate :: a -> IO ()
  
instance Draw (Double, Double) where
  draw (x,y) = renderPrimitive Points $ vertex $ Vertex3 x y 0

instance Draw (Double -> (Double, Double)) where
  draw g = renderPrimitive LineStrip $ forM_ [0,0.001..1] $ \t -> do
    let (x,y) = g t
    vertex $ Vertex3 x y 0
    
instance Draw (Double -> Double) where
  draw g = renderPrimitive Points $ forM_ [-1,-0.99..1] $ \x -> 
    vertex $ Vertex3 x (g x) 0

instance Draw a => Draw [a] where
  draw = mapM_ draw 

simpleDraw :: Draw a => a -> IO ()
simpleDraw obj = do
  (progname, _) <- getArgsAndInitialize
  createWindow "Draw"
  displayCallback $= display
  mainLoop 
  where
    display = do 
      clear [ColorBuffer]
      draw obj
      flush
  
instance Draw a => Animate (Double -> a) where
  animate obj = do
    (progname,_) <- getArgsAndInitialize  
    createWindow "Animate Function"
    t <- newIORef 0.0
    displayCallback $= (display t)
    idleCallback $= Just (idle t)
    mainLoop
    where
      idle time = do
        t <- get time
        time $=! (t + 0.01) -- The parens are necessary due to a precedence bug in StateVar
        postRedisplay Nothing
      display time = do
        clear [ColorBuffer]
        t <- get time
        draw $ obj t
        swapBuffers

instance Draw a => Animate [a] where
  animate objs = do
    (progname,_) <- getArgsAndInitialize
    createWindow "Animate Function"
    objsRef <- newIORef (cycle objs)
    let idle = do
          xs <- get objsRef
          objsRef $=! tail xs 
          postRedisplay Nothing
        display = do  
          xs <- get objsRef
          clear [ColorBuffer]
          draw (head xs)
          swapBuffers
    displayCallback $= display
    idleCallback $= Just idle
    mainLoop
