{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Main where

import SpriterTypes

import Control.Concurrent (threadDelay)
import Control.Lens ((^.))
import Control.Lens.TH (makeLenses)
import Control.Monad (unless, forM, forM_)

import Data.Map ((!), Map)
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.StateVar (($=))

import Foreign.C.Types (CDouble(..))
import Foreign.C.String (CString, withCString)
import Foreign.Marshal.Alloc (free)
import Foreign.Ptr (Ptr, nullPtr)

import Linear (V2(..), V4(..))

import qualified SDL
import qualified SDL.Image

import qualified Language.C.Inline.Cpp as C

C.context $ C.cppCtx <> C.funCtx <> spriterCtx

C.include "<spriterengine/spriterengine.h>"
C.include "<spriterengine/override/filefactory.h>"
C.include "<spriterengine/global/settings.h>"
C.include "SpriterHelpers.hpp"

C.using "namespace SpriterEngine"

winWidth, winHeight :: Num a => a
winWidth = 800
winHeight = 600

timeStep :: Double
timeStep = 1/60

data Sprite = Sprite
    { _spriteTexture :: SDL.Texture
    , _spriteWidth :: Int
    , _spriteHeight :: Int
    , _spritePivotX :: Double
    , _spritePivotY :: Double
    , _spriteName :: String
    }

makeLenses ''Sprite

type SpriterFolders = Map Int (Map Int Sprite)

data AnimState = AnimState
    { _frameTime :: Double
    }

makeLenses ''AnimState

loadSpriterModel :: CString -> IO (Ptr CSpriterModel)
loadSpriterModel modelPath =
    [C.exp| SpriterModel*
        { new SpriterModel($(char* modelPath), new SpriterFileFactory()) }|]

modelGetNewEntityInstance :: Ptr CSpriterModel -> CString -> IO (Ptr CEntityInstance)
modelGetNewEntityInstance model entityName =
    [C.exp| EntityInstance*
        { $(SpriterModel* model)->getNewEntityInstance($(char* entityName)) }|]

entityInstanceSetCurrentAnimation :: Ptr CEntityInstance -> CString -> IO ()
entityInstanceSetCurrentAnimation ptr animName =
    [C.exp| void
        { $(EntityInstance* ptr)->setCurrentAnimation($(char* animName)) } |]

printWithMsg :: CDouble -> IO ()
printWithMsg val =
    putStrLn $ "Val is " ++ show val ++ "."

main :: IO ()
main = do
    [C.exp| void { Settings::setErrorFunction(Settings::simpleError);} |]
    spriterModel <- withCString "res/CharacterTest/CharacterTest.scon" loadSpriterModel
    entityInstance <- withCString "Character" $ modelGetNewEntityInstance spriterModel
    withCString "Run" $ entityInstanceSetCurrentAnimation entityInstance

    SDL.initializeAll
    aaSucceded <-
        SDL.setHintWithPriority SDL.DefaultPriority SDL.HintRenderScaleQuality SDL.ScaleLinear

    unless aaSucceded $ putStrLn "Warning: Could not set anti-aliasing"

    window <- SDL.createWindow "Haskell Spriter Test" SDL.defaultWindow
        { SDL.windowInitialSize = V2 winWidth winHeight
        }

    [C.block|
     void {
         $fun:(void (*printWithMsg)(double))(4.0);
         $fun:(void (*printWithMsg)(double))(8.0);
     }
     |]

    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
--     folders <- loadFolders renderer (schema^.schemaFolder)

--     let chr = (schema^.schemaEntity) ! "Character"
--         idleAnim = (chr^.entityAnimation) ! "Idle"
--         runAnim = (chr^.entityAnimation) ! "Run"

    let
        apploop :: AnimState -> IO ()
        apploop state = do
            events <- SDL.pollEvents
            let qPressed = any eventIsQPress events

            SDL.rendererDrawColor renderer $= V4 0 0 0 255
            SDL.clear renderer

            SDL.present renderer

            threadDelay $ floor $ timeStep * 1000000
            unless qPressed $ apploop state
                { _frameTime = state ^. frameTime + timeStep
                }

    apploop
        AnimState
        { _frameTime = 0
        }

    free entityInstance
    free spriterModel

-- loadFolders :: SDL.Renderer -> [Sk.Folder] -> IO (Map Int (Map Int Sprite))
-- loadFolders renderer fs = Map.fromList <$> spriteList
--     where spriteList = forM fs $ \folder -> do
--               images <- loadImages renderer (folder^.folderFile)
--               return (folder^.folderId, images)

-- loadImages :: SDL.Renderer -> [Sk.File] -> IO (Map Int Sprite)
-- loadImages renderer fs = Map.fromList <$> spriteList
--     where spriteList = forM fs $ \file -> do
--               let filepath = "res/CharacterTest/" ++ file^.fileName
--               tex <- SDL.Image.loadTexture renderer $ filepath
--               putStrLn $ "Loaded " ++ filepath

--               let sprite = Sprite
--                       { _spriteTexture = tex
--                       , _spriteWidth = file ^. fileWidth
--                       , _spriteHeight = file ^. fileHeight
--                       , _spritePivotX = file ^. filePivotX
--                       , _spritePivotY = file ^. filePivotY
--                       , _spriteName = file ^. fileName
--                       }
--               return (file^.fileId, sprite)

eventIsQPress :: SDL.Event -> Bool
eventIsQPress event =
    case SDL.eventPayload event of
        SDL.KeyboardEvent keyboardEvent ->
            SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
            SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
        _ ->
            False

-- renderAnimation :: SDL.Renderer -> SpriterFolders -> [Sk.ResultBone] -> IO ()
-- renderAnimation renderer folders bs = forM_ bs $ \bone -> do
--     case bone ^. rbObj of
--         Nothing ->
--             return ()

--         Just boneObj ->
--             let sprite = folders ! (boneObj^.boneObjFolder) ! (boneObj^.boneObjFile)
--                 w = fromIntegral $ sprite ^. spriteWidth
--                 h = fromIntegral $ sprite ^. spriteHeight
--                 px = floor $ (sprite ^. spritePivotX) * fromIntegral w
--                 py = floor $ (1 - sprite ^. spritePivotY) * fromIntegral h
--                 pivot = Just $ SDL.P $ V2 px py
--                 angle = bone ^. rbAngle
--                 degAngle = angle * (- 180/pi)
--                 x = floor $ bone ^. rbX + 400 - fromIntegral px
--                 y = floor $ (- bone ^. rbY) + 400 - fromIntegral py
--                 texture = sprite ^. spriteTexture
--                 renderRect = SDL.Rectangle (SDL.P $ V2 x y) (V2 w h)
--             in
--                 SDL.copyEx
--                     renderer texture Nothing (Just $ renderRect) (CDouble degAngle) pivot (V2 False False)
