{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Main where

import Control.Concurrent (threadDelay)
import Control.Lens ((^.))
import Control.Lens.TH (makeLenses)
import Control.Monad (unless, forM, forM_)

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as B
import Data.Fixed (mod')
import Data.Map ((!), Map)
import qualified Data.Map as Map
import Data.StateVar (($=))

import Data.Spriter.Skeleton
    ( rbObj
    , rbX
    , rbY
    )
import qualified Data.Spriter.Skeleton as Sk

import Data.Spriter.Types
    ( animLength
    , boneObjFile
    , boneObjFolder
    , entityAnimation
    , fileId
    , fileName
    , fileWidth
    , fileHeight
    , folderFile
    , folderId
    , schemaEntity
    , schemaFolder
    )
import qualified Data.Spriter.Types as Sk

import Linear (V2(..), V4(..))

import qualified SDL
import qualified SDL.Image

winWidth, winHeight :: Num a => a
winWidth = 800
winHeight = 600

timeStep :: Double
timeStep = 1/60

data Sprite = Sprite
    { _spriteTexture :: SDL.Texture
    , _spriteWidth :: Int
    , _spriteHeight :: Int
    }

makeLenses ''Sprite

data Env = Env
    { _sdlRenderer :: SDL.Renderer
    , _spriterFolders :: SpriterFolders
    , _spriterSchema :: Sk.Schema
    , _character :: Sk.Entity
    , _idleAnimation :: Sk.Animation
    , _runAnimation :: Sk.Animation
    }

type SpriterFolders = Map Int (Map Int Sprite)

makeLenses ''Env

data AnimState = AnimState
    { _frameTime :: Double
    }

makeLenses ''AnimState

main :: IO ()
main = do
    characterJson <- B.readFile "res/CharacterTest/CharacterTest.scon"
    let characterDecoded = A.eitherDecode characterJson :: Either String Sk.Schema

    case characterDecoded of
        Left msg ->
            putStrLn $ "JSON parse failed" ++ msg

        Right schema -> do
            putStrLn "JSON loaded successfully"
            animateCharacter schema

animateCharacter :: Sk.Schema -> IO ()
animateCharacter schema = do
    SDL.initializeAll
    window <- SDL.createWindow "Haskell Spriter Test" SDL.defaultWindow
        { SDL.windowInitialSize = V2 winWidth winHeight
        }

    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    folders <- loadFolders renderer (schema^.schemaFolder)

    let chr = (schema^.schemaEntity) ! "Character"
        idleAnim = (chr^.entityAnimation) ! "Idle"
        runAnim = (chr^.entityAnimation) ! "Run"

    apploop
        Env
        { _sdlRenderer = renderer
        , _spriterSchema = schema
        , _spriterFolders = folders
        , _character = chr
        , _idleAnimation = idleAnim
        , _runAnimation = runAnim
        }

        AnimState
        { _frameTime = 0
        }

loadFolders :: SDL.Renderer -> [Sk.Folder] -> IO (Map Int (Map Int Sprite))
loadFolders renderer fs = Map.fromList <$> spriteList
    where spriteList = forM fs $ \folder -> do
              images <- loadImages renderer (folder^.folderFile)
              return (folder^.folderId, images)

loadImages :: SDL.Renderer -> [Sk.File] -> IO (Map Int Sprite)
loadImages renderer fs = Map.fromList <$> spriteList
    where spriteList = forM fs $ \file -> do
              let filepath = "res/CharacterTest/" ++ file^.fileName
              tex <- SDL.Image.loadTexture renderer $ filepath
              putStrLn $ "Loaded " ++ filepath

              let sprite = Sprite
                      { _spriteTexture = tex
                      , _spriteWidth = file ^. fileWidth
                      , _spriteHeight = file ^. fileHeight
                      }
              return (file^.fileId, sprite)

eventIsQPress :: SDL.Event -> Bool
eventIsQPress event =
    case SDL.eventPayload event of
        SDL.KeyboardEvent keyboardEvent ->
            SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
            SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
        _ ->
            False

apploop :: Env -> AnimState -> IO ()
apploop env state = do
    events <- SDL.pollEvents
    let qPressed = any eventIsQPress events
        frameTime' = (state^.frameTime + timeStep) `mod'` (env^.idleAnimation.animLength)
        animationResult = Sk.animate (env^.idleAnimation) frameTime'
        renderer = env ^. sdlRenderer

    SDL.rendererDrawColor renderer $= V4 0 0 0 255
    SDL.clear renderer

    case animationResult of
        Nothing ->
            putStrLn "ERROR: frameTime was longer than animation"

        Just resultBones ->
            renderAnimation renderer (env^.spriterFolders) resultBones

    SDL.present renderer

    threadDelay $ floor $ timeStep * 1000000
    unless qPressed $ apploop env state

renderAnimation :: SDL.Renderer -> SpriterFolders -> [Sk.ResultBone] -> IO ()
renderAnimation renderer folders bs = forM_ bs $ \bone -> do
    case bone ^. rbObj of
        Nothing ->
            return ()

        Just boneObj ->
            let sprite = folders ! (boneObj^.boneObjFolder) ! (boneObj^.boneObjFile)
                x = floor $ bone ^. rbX
                y = floor $ bone ^. rbY
                w = fromIntegral $ sprite ^. spriteWidth
                h = fromIntegral $ sprite ^. spriteHeight
                texture = sprite ^. spriteTexture
                renderRect = SDL.Rectangle (SDL.P $ V2 x y) (V2 w h)
            in SDL.copy renderer texture Nothing (Just $ renderRect)
