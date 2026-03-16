module Input where

import qualified Data.Map as M
import qualified Data.Set as Set
import Types
import qualified SDL
import Linear
import Apecs
import qualified Data.Map as Map

-- Bool in type is True for pressed, False for released
updateKeySet :: Ord rawKey => KeyBindings rawKey -> rawKey -> Bool -> Set.Set GameKey -> Set.Set GameKey
updateKeySet (KeyBindings m) rk pressed s = case M.lookup rk m of
    Nothing -> s
    Just key -> if pressed then
                    Set.insert key s
                else
                    Set.delete key s

inputBindings :: KeyBindings SDL.Keycode
inputBindings = KeyBindings $ Map.fromList [
        (SDL.KeycodeW, GkUp),
        (SDL.KeycodeS, GkDown),
        (SDL.KeycodeA, GkLeft),
        (SDL.KeycodeD, GkRight),
        (SDL.KeycodeSpace, GkSpace),
        (SDL.KeycodeEscape, GkEsc),
        (SDL.KeycodeE, GkE),
        (SDL.KeycodeQ, GkQ),
        (SDL.KeycodeF, GkF)
    ]

handlePayload :: [SDL.EventPayload] -> System' ()
handlePayload = mapM_ handleEvent

handleEvent :: SDL.EventPayload -> System' ()
handleEvent (SDL.KeyboardEvent ev) = handleKeyEvent ev
handleEvent (SDL.MouseMotionEvent ev) = handleMouseMotionEvent ev
handleEvent (SDL.MouseButtonEvent ev) = handleMouseButtonEvent ev
handleEvent _ = return ()

handleMouseButtonEvent :: SDL.MouseButtonEventData -> System' ()
handleMouseButtonEvent ev
    | SDL.mouseButtonEventMotion ev == SDL.Pressed && SDL.mouseButtonEventButton ev == SDL.ButtonLeft = modify global $ \(KeysPressed ks) -> KeysPressed $ Set.insert GkLMB ks
    | SDL.mouseButtonEventMotion ev == SDL.Released && SDL.mouseButtonEventButton ev == SDL.ButtonLeft = modify global $ \(KeysPressed ks) -> KeysPressed $ Set.delete GkLMB ks
    | otherwise = return ()

handleMouseMotionEvent :: SDL.MouseMotionEventData -> System' ()
handleMouseMotionEvent ev = let
        (SDL.P (V2 x y)) = SDL.mouseMotionEventPos ev
    in do
        Viewport (w, h) <- get global
        -- Normalise mouse coordinates to be relative to the center of the screen, and invert y-axis to match game coordinate system
        -- along with normalising for fullscreen applications relative to the base resolution of 1280x720
        let nx = fromIntegral x * (1280 / fromIntegral w)
            ny = (- fromIntegral y) * (720 / fromIntegral h)
        modify global $ \(MousePosition _) -> MousePosition (V2 nx ny)

handleKeyEvent :: SDL.KeyboardEventData -> System' ()
handleKeyEvent ev
    | SDL.keyboardEventKeyMotion ev == SDL.Pressed = modify global $ \(KeysPressed ks) -> KeysPressed $ updateKeySet inputBindings (SDL.keysymKeycode (SDL.keyboardEventKeysym ev)) True ks
    | SDL.keyboardEventKeyMotion ev == SDL.Released = modify global $ \(KeysPressed ks) -> KeysPressed $ updateKeySet inputBindings (SDL.keysymKeycode (SDL.keyboardEventKeysym ev)) False ks
    | otherwise = return ()