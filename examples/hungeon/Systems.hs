{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Systems where

import Apecs
import System.Random hiding (next)
import System.Exit
import Linear
import Control.Monad
import Types
import GameMap
import Data.Set (Set)
import qualified Data.Set as Set
import Utils
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import System.IO.Unsafe ( unsafePerformIO )
import Combat
import Dungeon
import Menu
import Settings
import qualified Data.Vector as V
import Data.Functor
import Data.Foldable
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified SDL
import GDK.Font
import GDK.Types
import GDK.Texture

-- Initialise the game state by creating a player entity
initialize :: SDL.Renderer -> System' ()
initialize r = do
    loadTexture r "examples/hungeon/assets/player/idle.png" "player-idle"  (Just Animation { frameCount = 6, frameSpeed = 0.3, next = "player-idle" })
    loadTexture r "examples/hungeon/assets/player/walk.png" "player-walk" (Just Animation { frameCount = 10, frameSpeed = 0.1, next = "player-walk" })
    loadTexture r "examples/hungeon/assets/player/knife-attack.png" "player-knife-attack" (Just Animation { frameCount = 9, frameSpeed = 0.1, next = "player-idle" })
    loadTexture r "examples/hungeon/assets/player/fire-attack.png" "player-fire-attack" (Just Animation { frameCount = 11, frameSpeed = 0.1, next = "player-idle" })
    loadTexture r "examples/hungeon/assets/player/electric-attack.png" "player-electric-attack" (Just Animation { frameCount = 11, frameSpeed = 0.1, next = "player-idle" })
    loadTexture r "examples/hungeon/assets/player/prismatic-attack.png" "player-prismatic-attack" (Just Animation { frameCount = 11, frameSpeed = 0.1, next = "player-idle" })
    loadTexture r "examples/hungeon/assets/player/hit.png" "player-hit" (Just Animation { frameCount = 5, frameSpeed = 0.1, next = "player-idle" })
    loadTexture r "examples/hungeon/assets/player/shield.png" "player-shield" (Just Animation { frameCount = 6, frameSpeed = 0.1, next = "player-idle" })
    loadTexture r "examples/hungeon/assets/enemies/skeleton/idle.png" "skeleton-idle" (Just Animation { frameCount = 6, frameSpeed = 0.3, next = "skeleton-idle" })
    loadTexture r "examples/hungeon/assets/enemies/skeleton/walk.png" "skeleton-walk" (Just Animation { frameCount = 10, frameSpeed = 0.1, next = "skeleton-walk" })
    loadTexture r "examples/hungeon/assets/enemies/skeleton/attack.png" "skeleton-attack" (Just Animation { frameCount = 9, frameSpeed = 0.1, next = "skeleton-idle" })
    loadTexture r "examples/hungeon/assets/enemies/skeleton/hit.png" "skeleton-hit" (Just Animation { frameCount = 5, frameSpeed = 0.1, next = "skeleton-idle" })
    loadTexture r "examples/hungeon/assets/enemies/skeleton/death.png" "skeleton-death" (Just Animation { frameCount = 17, frameSpeed = 0.1, next = "" })
    loadTexture r "examples/hungeon/assets/enemies/reaper/idle.png" "reaper-idle" (Just Animation { frameCount = 6, frameSpeed = 0.3, next = "reaper-idle" })
    loadTexture r "examples/hungeon/assets/enemies/reaper/walk.png" "reaper-walk" (Just Animation { frameCount = 10, frameSpeed = 0.1, next = "reaper-walk" })
    loadTexture r "examples/hungeon/assets/enemies/reaper/attack.png" "reaper-attack" (Just Animation { frameCount = 15, frameSpeed = 0.1, next = "reaper-idle" })
    loadTexture r "examples/hungeon/assets/enemies/reaper/hit.png" "reaper-hit" (Just Animation { frameCount = 5, frameSpeed = 0.1, next = "reaper-idle" })
    loadTexture r "examples/hungeon/assets/enemies/reaper/death.png" "reaper-death" (Just Animation { frameCount = 15, frameSpeed = 0.1, next = "" })
    loadTexture r "examples/hungeon/assets/enemies/vampire/idle.png" "vampire-idle" (Just Animation { frameCount = 6, frameSpeed = 0.3, next = "vampire-idle" })
    loadTexture r "examples/hungeon/assets/enemies/vampire/walk.png" "vampire-walk" (Just Animation { frameCount = 8, frameSpeed = 0.1, next = "vampire-walk" })
    loadTexture r "examples/hungeon/assets/enemies/vampire/attack.png" "vampire-attack" (Just Animation { frameCount = 16, frameSpeed = 0.1, next = "vampire-idle" })
    loadTexture r "examples/hungeon/assets/enemies/vampire/hit.png" "vampire-hit" (Just Animation { frameCount = 5, frameSpeed = 0.1, next = "vampire-idle" })
    loadTexture r "examples/hungeon/assets/enemies/vampire/death.png" "vampire-death" (Just Animation { frameCount = 14, frameSpeed = 0.1, next = "" })
    loadTexture r "examples/hungeon/assets/enemies/golden-reaper/idle.png" "golden-reaper-idle" (Just Animation { frameCount = 6, frameSpeed = 0.3, next = "golden-reaper-idle" })
    loadTexture r "examples/hungeon/assets/enemies/golden-reaper/walk.png" "golden-reaper-walk" (Just Animation { frameCount = 8, frameSpeed = 0.1, next = "golden-reaper-walk" })
    loadTexture r "examples/hungeon/assets/enemies/golden-reaper/attack.png" "golden-reaper-attack" (Just Animation { frameCount = 15, frameSpeed = 0.1, next = "golden-reaper-idle" })
    loadTexture r "examples/hungeon/assets/enemies/golden-reaper/hit.png" "golden-reaper-hit" (Just Animation { frameCount = 5, frameSpeed = 0.1, next = "golden-reaper-idle" })
    loadTexture r "examples/hungeon/assets/enemies/golden-reaper/death.png" "golden-reaper-death" (Just Animation { frameCount = 15, frameSpeed = 0.1, next = "" })
    loadTexture r "examples/hungeon/assets/particles/fire.png" "particle-fire" (Just Animation { frameCount = 75, frameSpeed = 1/60, next = "" })
    loadTexture r "examples/hungeon/assets/particles/prismatic.png" "particle-prismatic" (Just Animation { frameCount = 81, frameSpeed = 1/60, next = "" })
    loadTexture r "examples/hungeon/assets/tiles/wall-bottom-right.png" "wall-bottom-right" Nothing
    loadTexture r "examples/hungeon/assets/tiles/wall-bottom-left.png" "wall-bottom-left" Nothing
    loadTexture r "examples/hungeon/assets/ui/combat-ui.png" "combat-attack-select-ui" Nothing
    loadTexture r "examples/hungeon/assets/ui/combat-ui-magic.png" "combat-magic-select-ui" Nothing
    loadTexture r "examples/hungeon/assets/ui/combat-ui-parry.png" "combat-parry-ui" Nothing
    loadTexture r "examples/hungeon/assets/ui/transition.png" "transition" Nothing
    loadTexture r "examples/hungeon/assets/tiles/ladder.png" "ladder" Nothing
    loadTexture r "examples/hungeon/assets/items/heart.png" "heart" Nothing
    loadTexture r "examples/hungeon/assets/ui/title-screen.png" "title-screen"  Nothing
    loadTexture r "examples/hungeon/assets/ui/settings-screen.png" "settings-screen" Nothing
    loadTexture r "examples/hungeon/assets/ui/start-game/button.png" "start-game-button" Nothing
    loadTexture r "examples/hungeon/assets/ui/start-game/hover.png" "start-game-button-hover" Nothing
    loadTexture r "examples/hungeon/assets/ui/settings/button.png" "settings-button" Nothing
    loadTexture r "examples/hungeon/assets/ui/settings/hover.png" "settings-button-hover" Nothing
    loadTexture r "examples/hungeon/assets/ui/windowed/button.png" "windowed-button" Nothing
    loadTexture r "examples/hungeon/assets/ui/windowed/hover.png" "windowed-button-hover" Nothing
    loadTexture r "examples/hungeon/assets/ui/fullscreen/button.png" "fullscreen-button" Nothing
    loadTexture r "examples/hungeon/assets/ui/fullscreen/hover.png" "fullscreen-button-hover" Nothing
    loadTexture r "examples/hungeon/assets/ui/back/button.png" "back-button" Nothing
    loadTexture r "examples/hungeon/assets/ui/back/hover.png" "back-button-hover" Nothing
    let tiles = [ (ident, path)
                | n <- [1..tileCount]
                , let ident = "tile" ++ show n
                , let path = "examples/hungeon/assets/tiles/tile" ++ show n ++ ".png"
            ] ++
            [ (ident, path)
                | n <- [1..wallTopCount]
                , let ident = "wall-top" ++ show n
                , let path = "examples/hungeon/assets/tiles/wall-top" ++ show n ++ ".png"
            ] ++
            [ (ident, path)
                | n <- [1..wallBottomCount]
                , let ident = "wall-bottom" ++ show n
                , let path = "examples/hungeon/assets/tiles/wall-bottom" ++ show n ++ ".png"
            ] ++
            [ (ident, path)
                | n <- [1..wallLeftCount]
                , let ident = "wall-left" ++ show n
                , let path = "examples/hungeon/assets/tiles/wall-left" ++ show n ++ ".png"
            ] ++
            [ (ident, path)
                | n <- [1..wallRightCount]
                , let ident = "wall-right" ++ show n
                , let path = "examples/hungeon/assets/tiles/wall-right" ++ show n ++ ".png"
            ] ++
            [ (ident, path)
                | n <- [1..wallBottomLeftElbowCount]
                , let ident = "wall-bottom-left-elbow" ++ show n
                , let path = "examples/hungeon/assets/tiles/wall-bottom-left-elbow" ++ show n ++ ".png"
            ] ++
            [ (ident, path)
                | n <- [1..wallBottomRightElbowCount]
                , let ident = "wall-bottom-right-elbow" ++ show n
                , let path = "examples/hungeon/assets/tiles/wall-bottom-right-elbow" ++ show n ++ ".png"
            ]
    mapM_ (\(ident, path) -> loadTexture r path ident Nothing) tiles
    loadFont "examples/hungeon/assets/Roboto-Regular.ttf" "Roboto-Regular" 24
    settings <- liftIO (BL.readFile "examples/hungeon/settings.json" <&> decode) :: System' (Maybe Settings)
    liftIO $ print settings
    forM_ settings (set global)
    createMenuButtons
    createSettingsButtons
    void $ newEntity (TitleScreen, Texture RenTexture { textureRef = "title-screen", textureLayer = 0, animationFrame = Nothing, textureVisible = True }, Position (V2 0 0))

makeVisibile :: Renderable -> Renderable
makeVisibile (Texture t) = Texture t { textureVisible = True }
makeVisibile (Text t) = Text t { textVisible = True }
makeVisibile (Point p) = Point p { pointVisible = True }
makeVisibile (Line l) = Line l { lineVisible = True }
makeVisibile (Rectangle r) = Rectangle r { rectVisible = True }
makeVisibile (FilledRectangle r) = FilledRectangle r { rectVisible = True }

makeInvisible :: Renderable -> Renderable
makeInvisible (Texture t) = Texture t { textureVisible = False }
makeInvisible (Text t) = Text t { textVisible = False }
makeInvisible (Point p) = Point p { pointVisible = False }
makeInvisible (Line l) = Line l { lineVisible = False }
makeInvisible (Rectangle r) = Rectangle r { rectVisible = False }
makeInvisible (FilledRectangle r) = FilledRectangle r { rectVisible = False }

createSettingsButtons :: System' ()
createSettingsButtons = do
    settings <- get global
    windowedButton <- newEntity (SettingsUIElement, Button WindowedButton, Position (V2 (-132) 124), Texture RenTexture { textureRef="windowed-button", textureLayer = 1, animationFrame = Nothing, textureVisible = False })
    fullscreenButton <- newEntity (SettingsUIElement, Button FullscreenButton, Position (V2 135 124), Texture RenTexture { textureRef="fullscreen-button", textureLayer = 1, animationFrame = Nothing, textureVisible = False })
    _ <- newEntity (SettingsUIElement, Button BackToTitleButton, Position (V2 (-450) 300), Texture RenTexture { textureRef="back-button", textureLayer = 1, animationFrame = Nothing, textureVisible = False })
    let startActiveButton = case settings of
            Just (Settings { fullscreen = True }) -> fullscreenButton
            _ -> windowedButton
    void $ newEntity (SettingsUIElement, ButtonGroup (V.fromList [windowedButton, fullscreenButton]) startActiveButton)

createMenuButtons :: System' ()
createMenuButtons = do
    _ <- newEntity (MainMenuUIElement, Button StartGameButton, Position (V2 (1280/2 - 50 - 100) (-400)), Texture RenTexture { textureRef="start-game-button", textureLayer = 1, animationFrame = Nothing, textureVisible = True })
    void $ newEntity (MainMenuUIElement, Button SettingsButton, Position (V2 (1280/2 - 100) (-500)), Texture RenTexture { textureRef="settings-button", textureLayer = 1, animationFrame = Nothing, textureVisible = True })

incrementTime :: Float -> System' ()
incrementTime dT = do
    modify global $ \(ShieldCooldown sc) -> ShieldCooldown (max 0 (sc - dT))

-- Remove Velocity component from particles whose destination position has been reached
-- When a particle finishes its animation, remove it from the world
stepParticles :: Float -> System' ()
stepParticles dT = cmapM_ $ \(Particle (Position destP), Position currP, r, e) -> case r of
    Texture rt -> do
        TextureMap tmap <- get global
        when (norm (destP - currP) < 5) $ do
            destroy e (Proxy @Velocity)
        let TextureData _ rs = tmap Map.! textureRef rt
        case rs of
            Just a -> when (fromMaybe 0 (animationFrame rt) + 1 >= frameCount a) $ do
                destroy e (Proxy @(Particle, Renderable, Position))
                cmapM_ $ \(CombatAttackParticle _, e') -> destroy e' (Proxy @CombatAttackParticle)
            Nothing -> return ()
    _ -> return ()


triggerEvery :: Float -> Float -> Float -> System' a -> System' ()
triggerEvery dT period offset sys = do
    Time t <- get global
    let t' = t + offset
        trigger = floor (t'/period) /= floor ((t'+dT)/period)
    when trigger $ void sys

toDungeonAction :: System' ()
toDungeonAction = do
    ce <- cfold (\_ (CombatEnemy ce) -> Just ce) Nothing
    case ce of
        Nothing -> return ()
        Just e -> do
            destroy e (Proxy @(Enemy, Renderable, Position, Velocity, Health))
            cmapM_ $ \(CombatEnemy _, e') -> destroy e' (Proxy @(CombatEnemy, Renderable, Position))
            set global DungeonState
    cmap $ \(Player, r) -> makeVisibile r
    cmap $ \(Enemy _, r) -> makeVisibile r
    cmap $ \(Tile, r) -> makeVisibile r
    cmap $ \(Wall, r) -> makeVisibile r
    cmap $ \(Ladder, r) -> makeVisibile r
    cmap $ \(Heart, r) -> makeVisibile r
    cmap $ \(CombatPlayer, r) -> makeInvisible r
    cmap $ \(CombatWall, r) -> makeInvisible r
    cmap $ \(CombatTile, r) -> makeInvisible r
    cmapM_ $ \(CombatUI, e) -> destroy e (Proxy @(CombatUI, Position, Renderable))

toCombatAction :: System' ()
toCombatAction = do
    cmap $ \(Player, r) -> makeInvisible r
    cmap $ \(Enemy _, r) -> makeInvisible r
    cmap $ \(Tile, r) -> makeInvisible r
    cmap $ \(Wall, r) -> makeInvisible r
    cmap $ \(Ladder, r) -> makeInvisible r
    cmap $ \(Heart, r) -> makeInvisible r
    cmap $ \(CombatPlayer, r) -> makeVisibile r
    cmap $ \(CombatWall, r) -> makeVisibile r
    cmap $ \(CombatTile, r) -> makeVisibile r
    _ <- newEntity (CombatUI, Position (V2 0 0), Texture RenTexture { textureRef = "combat-attack-select-ui", textureLayer = 3, animationFrame = Nothing, textureVisible = True })
    set global CombatState

toNextLevelAction :: System' ()
toNextLevelAction = do
    -- Destroy all Walls, Floors, etc.
    cmapM_ $ \(Wall, e) -> destroy e (Proxy @(Wall, Tile, Position, Renderable, BoundaryBox))
    cmapM_ $ \(Ladder, e) -> destroy e (Proxy @(Ladder, Tile, Position, Renderable, BoundaryBox))
    cmapM_ $ \(Tile, e) -> destroy e (Proxy @(Tile, Position, Renderable))
    cmapM_ $ \(Enemy _, e) -> destroy e (Proxy @(Enemy, Position, Velocity, Health, Renderable, BoundaryBox))
    cmapM_ $ \(Heart, Item, e) -> destroy e (Proxy @(Heart, Item, Position, Renderable, BoundaryBox))
    cmap $ \(Player, Position _) -> Position playerPos
    generateMap

startDungeonAction :: System' ()
startDungeonAction = do
    _ <- newEntity (Player, Position playerPos, Velocity (V2 0 0), Texture RenTexture { textureRef = "player-idle", textureLayer = 2, animationFrame = Just 0, textureVisible = True },  BoundaryBox (16, 26) (0, -11), Health 100)
    _ <- newEntity (CombatPlayer, Position combatPlayerPos, Texture RenTexture { textureRef = "player-idle", textureLayer = 2, animationFrame = Just 0, textureVisible = False })
    generateMap
    let offsetX = tileSize / 2 - 1280/2
        offsetY = tileSize / 2 - 720/2
        getTileSprite :: IO String
        getTileSprite = do
            n <- randomRIO (1,tileCount) :: IO Integer
            return $ "tile" ++ show n
    tileList <- liftIO $ sequence [ do
        t <- getTileSprite
        let sref = Texture RenTexture { textureRef = t, textureLayer = 0, animationFrame = Nothing, textureVisible = False }
            pos = Position (V2 (fromIntegral x * tileSize) (- fromIntegral y * tileSize))

        return (sref, pos)
        | x <- [0..ceiling (1280 / tileSize)], y <- [0..ceiling (720 / tileSize)] ]
    forM_ tileList $ \(s, p) -> void $ newEntity (CombatTile, p, s)
    cmap $ \(TitleScreen, r) -> makeInvisible r
    cmap $ \(MainMenuUIElement, r) -> makeInvisible r
    cmap $ \(SettingsUIElement, r) -> makeInvisible r
    set global DungeonState

toMenuAction :: System' ()
toMenuAction = do
    -- Destroy all entities except the transition
    -- Destroy all map entities
    cmapM_ $ \(Wall, e) -> destroy e (Proxy @(Wall, Tile, Position, Renderable, BoundaryBox))
    cmapM_ $ \(Ladder, e) -> destroy e (Proxy @(Ladder, Tile, Position, Renderable, BoundaryBox))
    cmapM_ $ \(Tile, e) -> destroy e (Proxy @(Tile, Position, Renderable))
    cmapM_ $ \(Enemy _, e) -> destroy e (Proxy @(Enemy, Position, Velocity, Health, Renderable, BoundaryBox))
    cmapM_ $ \(Heart, Item, e) -> destroy e (Proxy @(Heart, Item, Position, Renderable, BoundaryBox))
    -- Destroy player entity
    cmapM_ $ \(Player, e) -> destroy e (Proxy @(Player, Position, Velocity, Renderable, BoundaryBox, Health))
    -- Destroy combat entities
    cmapM_ $ \(CombatPlayer, e) -> destroy e (Proxy @(CombatPlayer, Position, Renderable))
    cmapM_ $ \(CombatEnemy _, e) -> destroy e (Proxy @(CombatEnemy, Position, Renderable))
    -- Destroy Settings menu entities
    cmap $ \(SettingsUIElement, r) -> makeInvisible r
    cmap $ \(MainMenuUIElement, r) -> makeVisibile r
    cmap $ \(TitleScreen, r) -> makeVisibile r
    cmapM_ $ \(CombatUI, e) -> destroy e (Proxy @(CombatUI, Position, Renderable))
    set global MenuState

stepTransition :: Float -> System' ()
stepTransition dT = cmapM_ $ \(Transition p ang spd fired event, e) -> do
    let p' = p + dT * spd
    when (not fired && p' >= 0.5) $ case event of
        ToCombat -> toCombatAction
        ToDungeon -> toDungeonAction
        ToNextLevel -> toNextLevelAction
        StartDungeon -> startDungeonAction
        ToMenu -> toMenuAction
        ToSettings -> do
            cmap $ \(MainMenuUIElement, r) -> makeInvisible r
            cmap $ \(SettingsUIElement, r) -> makeVisibile r
            set global SettingsState
    if p' >= 1 then
        destroy e (Proxy @(Transition, Position, Renderable))
    else do
        set e Transition { trProgress = p', trAngle = ang, trSpeed = spd, trCoverEventFired = fired || p' >= 0.5, trEvent = event }
        let t = easeInOut (min 1 p)
            dist = Utils.lerp (-2000) 2000 t
            dx = dist * cos ang
            dy = dist * sin ang
        player <- cfold (\_ (Player, Position p, r) -> if isVisible r then Just p else Nothing) Nothing
        let pos = case player of
                Just (V2 px py) -> V2 (dx - 1300 + px - 1280/2 + 32) (dy + 1200 - (- py - 720/2 + 32))
                Nothing -> V2 (dx - 1300) (dy + 1200)
        set e $ Position pos

stepFloatingText :: Float -> System' ()
stepFloatingText dt = cmapM_ $ \(ft, e) -> if currLifetime ft + dt >= lifetime ft then
        destroy e (Proxy @(FloatingText, Position, Renderable, Velocity))
    else
        set e $ ft { currLifetime = currLifetime ft + dt }

step :: Float -> System' ()
step dT = do
    gs <- get global
    player <- cfold (\_ (Player, Position p, r) -> if isVisible r then Just p else Nothing) Nothing
    let func (V2 x y) = case player of
            Just (V2 px py) -> V2 (x - px + 1280/2 - 32) ((-y) + py + 720/2 - 32)
            Nothing -> V2 x (-y)
    set global $ Camera func
    incrementTime dT
    stepParticles dT
    stepTransition dT
    stepFloatingText dT
    case gs of
        DungeonState -> stepDungeon dT
        CombatState  -> stepCombat dT
        MenuState -> stepMenu dT
        SettingsState -> stepSettings dT
        _            -> return ()

isVisible :: Renderable -> Bool
isVisible (Texture t) = textureVisible t
isVisible (Text t) = textVisible t
isVisible (Point p) = pointVisible p
isVisible (Line l) = lineVisible l
isVisible (Rectangle r) = rectVisible r
isVisible (FilledRectangle r) = rectVisible r