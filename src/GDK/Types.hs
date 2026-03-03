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
                 , FPS
                 , Position(..)
                 , Time(..)
                 , Renderer(..)
                 , Window(..)) where

import qualified SDL
import Apecs
import Data.Word (Word8)
import GDK.Texture (RenTexture, TextureMap (..))
import GDK.Font (RenText)

type FPS = Int

-- | Configuration settings for the game upon initialisation
data Config = Config
    {
        windowTitle :: String, -- ^ Title of the game window
        windowDimensions :: (Int, Int), -- ^ Width and height of the game window in pixels
        backgroundColor :: SDL.V4 Word8, -- ^ Background color of the game window as an RGBA value
        targetFPS :: FPS -- ^ Desired FPS for the game loop
    }

-- data Render r = Render r

-- class Renderable r w where
--     render :: SDL.Renderer -> r -> System w ()
-- instance Renderable r w => Component (Render r) where type Storage (Render r) = Map (Render r)


-- class Component a => Renderable a where
--     render :: forall w. (Has w IO TextureMap) => SDL.Renderer -> Position -> a -> System w ()

-- data RenderableTexture = RenderableTexture RenTexture
-- instance Renderable RenderableTexture where
--     render r pos (RenderableTexture t) = do
--         TextureMap tm <- get global
--         return ()

-- | Represents an entity that can be rendered
data Renderable = RenderableTexture RenTexture
                | RenderableText RenText
                deriving (Eq, Show)
instance Component Renderable where type Storage Renderable = Map Renderable

newtype Position = Position (SDL.V2 Float)
instance Component Position where type Storage Position = Map Position

newtype Time = Time Float deriving (Show, Eq, Num)
instance Semigroup Time where
    (Time t1) <> (Time t2) = Time (t1 + t2)
instance Monoid Time where
    mempty = Time 0
instance Component Time where type Storage Time = Global Time

newtype Renderer = Renderer SDL.Renderer
instance Component Renderer where type Storage Renderer = Global Renderer

newtype Window = Window SDL.Window
instance Component Window where type Storage Window = Global Window