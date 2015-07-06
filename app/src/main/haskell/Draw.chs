module Draw (init, quit, render) where

import Prelude hiding (log, init)
import Data.Word (Word32)
import Foreign
import Foreign.C
import Data.Bits ((.|.))

import qualified Graphics.UI.SDL.Basic as SDL
import qualified Graphics.UI.SDL.Video as SDL
import qualified Graphics.UI.SDL.Types as SDL
import Graphics.UI.SDL.Enum

import Android (LogPriority(..))
import qualified Android
import System.Exit (exitSuccess, exitFailure)

#include <GLES2/gl2.h>

{#fun glClearColor { `Float', `Float', `Float', `Float' } -> `()' #}

log :: String -> IO ()
log = Android.log AndroidLogDebug "AgarIO.Draw"

die :: String -> IO ()
die s = do
  log s
  exitFailure


init :: IO ()
init = do
  result <- SDL.init (SDL_INIT_TIMER .|. SDL_INIT_VIDEO .|. SDL_INIT_EVENTS)
  if result /= 0 then die ("Failed to initialize SDL: " ++ show result) else do
    log "SDL properly initialized!"
    SDL.glSetAttribute SDL_GL_CONTEXT_MAJOR_VERSION (fromIntegral 2)
    SDL.glSetAttribute SDL_GL_CONTEXT_MINOR_VERSION (fromIntegral 0)
    SDL.glSetAttribute SDL_GL_CONTEXT_PROFILE_MASK SDL_GL_CONTEXT_PROFILE_ES
    s <- newCString ""
    window <- SDL.createWindow s
              SDL_WINDOWPOS_UNDEFINED SDL_WINDOWPOS_UNDEFINED 0 0
              (SDL_WINDOW_FULLSCREEN_DESKTOP .|. SDL_WINDOW_OPENGL .|.
               SDL_WINDOW_BORDERLESS .|. SDL_WINDOW_INPUT_FOCUS .|.
               SDL_WINDOW_SHOWN)
    free s
    if window == nullPtr then die "Failed to open SDL window" else do
      context <- SDL.glCreateContext window
      if context == nullPtr then die "Failed to create OpenGL context" else do
        result <- SDL.glSetSwapInterval (fromIntegral 1)
        if result /= 0 then die ("Unable to set VSync: " ++ show result) else do
          glClearColor 0 1 0 1
          render

quit :: IO ()
quit = do
  log "Quitting..."
  SDL.glGetCurrentContext >>= SDL.glDeleteContext 
  log "Deleted context"
  SDL.glGetCurrentWindow >>= SDL.destroyWindow
  log "Deleted window"
  SDL.quit
  log "Quit SDL"
  exitSuccess

render :: IO ()
render = do
  log "Rendering window"
  {#call glClear #} {#const GL_COLOR_BUFFER_BIT #}
  SDL.glGetCurrentWindow >>= SDL.glSwapWindow
