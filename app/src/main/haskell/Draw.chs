module Draw (init, quit) where

import Prelude hiding (log, init)
import Data.Word (Word32)
import Foreign
import Foreign.C
import Foreign.Ptr (nullPtr)
import Foreign.Marshal.Alloc (malloc, free)
import Data.Bits ((.|.))

import qualified Graphics.UI.SDL.Basic as SDL
import qualified Graphics.UI.SDL.Video as SDL
import qualified Graphics.UI.SDL.Types as SDL
import qualified Graphics.UI.SDL.Event as SDL
import Graphics.UI.SDL.Enum

import Android (LogPriority(..))
import qualified Android
import System.Exit (exitSuccess, exitFailure)
import System.Mem (performGC)
import System.IO.Unsafe (unsafePerformIO)

import Control.Concurrent.STM.TVar (newTVarIO, readTVarIO, writeTVar, TVar)
import Control.Monad.STM (atomically)
import Control.Concurrent (forkIO)

#include <GLES2/gl2.h>

{#fun glClearColor { `Float', `Float', `Float', `Float' } -> `()' #}

log :: String -> IO ()
log = Android.log AndroidLogDebug "AgarIO.Draw"

die :: String -> IO ()
die s = do
  log s
  exitFailure

shouldRender :: TVar Bool
shouldRender = unsafePerformIO $ newTVarIO True

type EventFilter = Ptr () -> Ptr SDL.Event -> IO CInt

eventFilter :: EventFilter
eventFilter _ eventPtr = do
  event <- peek eventPtr
  determineAction $ SDL.eventType event
  where determineAction :: Word32 -> IO CInt
        determineAction e
          | e == SDL_APP_TERMINATING = do
            log "SDL_APP_TERMINATING handled"
            atomically $ writeTVar shouldRender False
--            quit
            return' 0

          | e == SDL_APP_LOWMEMORY = do
            performGC
            return' 0

          | e == SDL_APP_WILLENTERBACKGROUND = do
            log "SDL_APP_WILL BACKGROUND handled"
            atomically $ writeTVar shouldRender False
            return' 0

          | e == SDL_APP_DIDENTERFOREGROUND = do
            log "SDL_APP_DID FOREGROUND handled"
            atomically $ writeTVar shouldRender True
            return' 0

          | e == SDL_APP_WILLENTERFOREGROUND = do
            log "SDL_APP_WILL FOREGROUN handled"
            return' 0
          | e == SDL_APP_DIDENTERBACKGROUND = do
            log "SDL_APP_DID BACKGROUND handled"
            return' 0
          | otherwise = return' 1

        return' = return . fromIntegral

foreign import ccall "wrapper" mkEventFilter :: EventFilter -> IO (FunPtr EventFilter)

init :: IO ()
init = do
  result <- SDL.init (SDL_INIT_TIMER .|. SDL_INIT_VIDEO .|. SDL_INIT_EVENTS)
  if result /= 0 then die ("Failed to initialize SDL: " ++ show result) else do
    log "SDL properly initialized!"
    f <- mkEventFilter eventFilter
    SDL.setEventFilter f nullPtr
    startRendering

startRendering :: IO ()
startRendering = do
--  w <- readTVarIO window
--  if w == nullPtr then do
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

handleEvents :: IO ()
handleEvents = do
  eventPtr <- malloc
  res <- SDL.pollEvent eventPtr
  event <- peek eventPtr
  determineAction $ SDL.eventType event
  if res == 0 then return () else handleEvents
  where determineAction :: Word32 -> IO ()
        determineAction e
          | e == SDL_QUIT = do
            log "SDL_QUIT handled"
            atomically $ writeTVar shouldRender False
            quit
          | e == SDL_APP_LOWMEMORY = do
            performGC
          | otherwise = return ()


render :: IO ()
render = do
  shRend <- readTVarIO shouldRender
  if True then do
    {#call glClear #} {#const GL_COLOR_BUFFER_BIT #}
    SDL.glGetCurrentWindow >>= SDL.glSwapWindow
    handleEvents
    log "rendering"
  else
    log "Not rendering"
  render
