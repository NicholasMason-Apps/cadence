{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Test.Hspec
import Test.QuickCheck
import GDK.Systems (initialise, run, makeWorld', defaultConfig)
import Apecs
import GDK.Types (Config(..), Renderable(..), Position(..), Time(..), Renderer(..), Window(..))
import qualified SDL
import qualified Data.Text as T
import qualified Data.Map as Map

makeWorld' []

testConfig = Config "Test Window" (800, 600) (SDL.V4 0 0 0 255) 60

main :: IO ()
main = hspec $ do
    describe "GDK.Systems.initialise" $ do
        it "initialises SDL and creates a window and renderer" $ do
            w <- initWorld
            (window, renderer) <- initialise w testConfig
            size <- SDL.get (SDL.windowSize window)
            size `shouldBe` (SDL.V2 800 600)
            title <- SDL.get (SDL.windowTitle window)
            title `shouldBe` (T.pack "Test Window")
            SDL.destroyRenderer renderer
            SDL.destroyWindow window
    describe $ "GDK.Systems.stepAnimations" $ do
        it "Correctly steps a single animation frame" $ do
            w <- initWorld
            (window, renderer) <- initialise w testConfig
            tex <- SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessStatic (SDL.V2 0 0)
            let test1 = TextureData { texture = tex, animation = Animation { frameCount = 2, frameSpeed = 0.1, next = "test2" }}
                test2 = TextureData { texture = tex, animation = Animation { frameCount = 2, frameSpeed = 0.1, next = "test1" }}
            modify global $ \(TextureMap ts) -> Map.insert "test2" test2 (Map.insert "test1" test1 ts)
            _ <- newEntity (Texture (RenTexture { textureRef = "test1", textureLayer = 0, animationFrame = Just 0 }))
            stepAnimations 0.1
            en <- cfold (\acc r -> case r of
                    Texture t -> Just t
                    _ -> acc) Nothing
            en `shouldBe` (Just $ RenTexture { textureRef = "test1", textureLayer = 0, animationFrame = Just 1 })
        it "Correctly loops to the next animation" $ do
            w <- initWorld
            (window, renderer) <- initialise w testConfig
            tex <- SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessStatic (SDL.V2 0 0)
            let test1 = TextureData { texture = tex, animation = Animation { frameCount = 2, frameSpeed = 0.1, next = "test2" }}
                test2 = TextureData { texture = tex, animation = Animation { frameCount = 2, frameSpeed = 0.1, next = "test1" }}
            modify global $ \(TextureMap ts) -> Map.insert "test2" test2 (Map.insert "test1" test1 ts)
            _ <- newEntity (Texture (RenTexture { textureRef = "test1", textureLayer = 0, animationFrame = Just 0 }))
            stepAnimations 0.1
            stepAnimations 0.1
            stepAnimations 0.1
            en <- cfold (\acc r -> case r of
                    Texture t -> Just t
                    _ -> acc) Nothing
            en `shouldBe` (Just $ RenTexture { textureRef = "test2", textureLayer = 0, animationFrame = Just 0 })