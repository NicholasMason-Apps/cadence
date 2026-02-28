module GDK.Systems where

import Apecs
import GDK.Types
import Language.Haskell.TH.Syntax
import qualified SDL
import qualified SDL.Font as TTF
import qualified SDL.Image as IMG
import qualified Data.Text as T
import Data.Word (Word8)
import System.Exit (exitSuccess)
import Control.Monad (unless)

initialise :: String -- ^ Window title
           -> (Int, Int) -- ^ Initial window dimensions (width, height)
           -> IO (SDL.Window, SDL.Renderer) -- ^ Returns the created window and renderer contexts
initialise title (w,h) = do
    SDL.initialize [SDL.InitVideo]
    TTF.initialize
    IMG.initialize []
    let windowConfig = SDL.defaultWindow { SDL.windowInitialSize = SDL.V2 (fromIntegral w) (fromIntegral h),
                                           SDL.windowMode = SDL.Windowed,
                                           SDL.windowResizable = False }
    window <- SDL.createWindow (T.pack title) windowConfig

    let rendererConfig = SDL.defaultRenderer { SDL.rendererType = SDL.AcceleratedVSyncRenderer,
                                               SDL.rendererTargetTexture = False }
    renderer <- SDL.createRenderer window (-1) rendererConfig

    return (window, renderer)

run :: w -- ^ Initial world state
     -> SDL.Renderer -- ^ SDL renderer context
     -> SDL.Window -- ^ SDL window context
     -> SDL.V4 Word8 -- ^ Background color for the renderer (RGBA)
     -> (Float -> System w ()) -- ^ World step function
     -> ([SDL.EventPayload] -> System w ()) -- ^ Event handler
     -> (SDL.Renderer -> FPS -> System w ()) -- ^ Draw function, receives the renderer and current FPS
     -> IO ()
run world r w c step eventHandler draw = do
    SDL.showWindow w
    let loop prevTicks tickAcc fpsAcc = do
            ticks <- SDL.ticks
            payload <- map SDL.eventPayload <$> SDL.pollEvents
            let quit = SDL.QuitEvent `elem` payload
                dt = ticks - prevTicks
                tickAcc' = tickAcc + dt
                avgFps = 1000.0 / (fromIntegral tickAcc' / fromIntegral fpsAcc)
            runSystem (eventHandler payload) world
            runSystem (step $ fromIntegral dt / 1000) world
            SDL.rendererDrawColor r SDL.$= c
            SDL.clear r
            runSystem (draw r (round avgFps)) world
            SDL.present r
            unless quit $ loop ticks tickAcc' (fpsAcc + 1)
    loop 0 0 0
    SDL.destroyRenderer r
    SDL.destroyWindow w
    TTF.quit
    IMG.quit
    SDL.quit
    exitSuccess