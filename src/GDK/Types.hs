{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module GDK.Types (Config(..)
                 , Renderable(..)
                 , RenderLayers(..)
                 , FPS
                 , Time(..)) where

import qualified SDL
import Apecs
import qualified Data.Vector as V
import Data.Word (Word8)

type FPS = Int

-- | Configuration settings for the game upon initialisation
data Config = Config
    {
        windowTitle :: String, -- ^ Title of the game window
        windowDimensions :: (Int, Int), -- ^ Width and height of the game window in pixels
        backgroundColor :: SDL.V4 Word8, -- ^ Background color of the game window as an RGBA value
        targetFPS :: FPS -- ^ Desired FPS for the game loop
    }

-- | Represents an entity that can be rendered, containing a reference to its texture and optional animation data
data Renderable = Renderable
    { textureRef :: String
    -- ^ Identifier for the texture to render
    , layer :: Int
    -- ^ layer for draw order
    , animationFrame :: Maybe Int
    -- ^ Frame index for animations, if applicable
    } deriving (Eq, Show)

newtype Time = Time Float deriving (Show, Eq, Num)
instance Semigroup Time where
    (Time t1) <> (Time t2) = Time (t1 + t2)
instance Monoid Time where
    mempty = Time 0
instance Component Time where type Storage Time = Global Time

{-|
A global component which stores in a Vector a 'Texture' for each render layer.
The index of the vector corresponds to the render layer, so the texture at index 0 is the texture for render layer 0, and so on.
Lower layers are drawn first, and therefore can appear behind higher layers.
The texture for each layer is cleared at the start of each frame, and 'Renderables' are drawn onto the appropriate layer texture during the draw phase of the game loop.
-}
newtype RenderLayers = RenderLayers (V.Vector SDL.Texture)
instance Semigroup RenderLayers where
    (RenderLayers q1) <> (RenderLayers q2) = RenderLayers (q1 V.++ q2)
instance Monoid RenderLayers where
    mempty = RenderLayers V.empty
instance Component RenderLayers where type Storage RenderLayers = Global RenderLayers
