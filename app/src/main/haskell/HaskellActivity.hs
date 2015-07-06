{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellActivity where

import Prelude hiding (log)
import Android (log, LogPriority(..))
import Foreign
import Foreign.C
import Foreign.Marshal.Alloc (free)
import Control.Applicative ((<*>), (<$>))
import qualified Draw
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)

import Graphics.UI.SDL.Event as SDL
import Graphics.UI.SDL.Types as SDL

foreign export ccall "hsMain" hsMain :: IO ()
hsMain = do
  Draw.init
  loop
  Draw.quit

loop :: IO ()
loop = do
  SDL.pumpEvents
  Draw.render
  loop
