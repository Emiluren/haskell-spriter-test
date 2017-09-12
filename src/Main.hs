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
import Foreign.C.String (CString, withCString, peekCString)
import Foreign.Marshal.Alloc (free)
import Foreign.Ptr (Ptr(..), nullPtr, castPtr, FunPtr, freeHaskellFunPtr)
import Foreign.StablePtr
    ( StablePtr
    , newStablePtr
    , deRefStablePtr
    , castStablePtrToPtr
    , castPtrToStablePtr
    )
import Foreign.Storable

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

makeLenses ''Sprite
makeLenses ''SpriterPoint
makeLenses ''CSpriteState

type SpriterFolders = Map Int (Map Int Sprite)

type ImageLoader = CString -> CDouble -> CDouble -> IO (Ptr Sprite)
type Renderer = Ptr Sprite -> Ptr CSpriteState -> IO ()

loadSpriterModel :: (FunPtr ImageLoader) -> (FunPtr Renderer) -> CString -> IO (Ptr CSpriterModel)
loadSpriterModel imgloader renderer modelPath =
    [C.exp| SpriterModel*
        {
            new SpriterModel(
                $(char* modelPath),
                new SpriterFileFactory(
                        $(HaskellSprite* (*imgloader)(const char*, double, double)),
                        $(void (*renderer)(HaskellSprite*, SpriteState*))
                )
          )
        }|]

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
    putStrLn $ "Value is " ++ show val ++ "."

loadImage :: SDL.Renderer -> CString -> CDouble -> CDouble -> IO (Ptr Sprite)
loadImage renderer filename pivotX pivotY = do
    name <- peekCString filename

    tex <- SDL.Image.loadTexture renderer $ name
    putStrLn $ "Loaded " ++ name

    let sprite = Sprite
            { _spriteTexture = tex
            , _spritePivotX = pivotX
            , _spritePivotY = pivotY
            , _spriteName = name
            }

    stablePtr <- newStablePtr sprite

    -- TODO: Memory leak, make sure stablePtr is freed on exit
    return $ castPtr $ castStablePtrToPtr stablePtr

main :: IO ()
main = do
    SDL.initializeAll
    aaSucceded <-
        SDL.setHintWithPriority SDL.DefaultPriority SDL.HintRenderScaleQuality SDL.ScaleLinear

    unless aaSucceded $ putStrLn "Warning: Could not set anti-aliasing"

    window <- SDL.createWindow "Haskell Spriter Test" SDL.defaultWindow
        { SDL.windowInitialSize = V2 winWidth winHeight
        }

    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

    let
        sdlLoad = loadImage renderer
        sdlRender = renderSprite renderer

    imgloader <- $(C.mkFunPtr [t| ImageLoader |]) sdlLoad
    renderf <- $(C.mkFunPtr [t| Renderer |]) sdlRender

    [C.exp| void { Settings::setErrorFunction(Settings::simpleError); } |]
    spriterModel <- withCString "res/CharacterTest/CharacterTest.scon"
        (loadSpriterModel imgloader renderf)
    entityInstance <- withCString "Character" $ modelGetNewEntityInstance spriterModel
    withCString "Run" $ entityInstanceSetCurrentAnimation entityInstance

    let
        cTimeStep = CDouble $ timeStep * 1000

        apploop :: IO ()
        apploop = do
            events <- SDL.pollEvents
            let qPressed = any eventIsQPress events

            SDL.rendererDrawColor renderer $= V4 0 0 0 255
            SDL.clear renderer

            [C.block| void
             {
                 auto ent = $(EntityInstance* entityInstance);
                 ent->setTimeElapsed($(double cTimeStep));
                 ent->render();
             }
            |]

            SDL.present renderer

            threadDelay $ floor $ timeStep * 1000000
            unless qPressed $ apploop

    apploop

    free entityInstance
    free spriterModel
    freeHaskellFunPtr imgloader
    freeHaskellFunPtr renderf

eventIsQPress :: SDL.Event -> Bool
eventIsQPress event =
    case SDL.eventPayload event of
        SDL.KeyboardEvent keyboardEvent ->
            SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
            SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
        _ ->
            False

renderSprite :: SDL.Renderer -> Renderer
renderSprite renderer spritePtr spriteStatePtr = do
    sprite <- deRefStablePtr $ castPtrToStablePtr $ castPtr $ spritePtr
    spriteState <- peek spriteStatePtr

    textureInfo <- SDL.queryTexture $ sprite ^. spriteTexture
    --putStrLn $ "rendering: " ++ sprite ^. spriteName
    --print spriteState
    let w = fromIntegral $ SDL.textureWidth textureInfo
        h = fromIntegral $ SDL.textureHeight textureInfo
        px = floor $ (sprite ^. spritePivotX) * fromIntegral w
        py = floor $ (sprite ^. spritePivotY) * fromIntegral h
        pivot = Just $ SDL.P $ V2 px py
        angle = spriteState ^. spriteStateAngle
        degAngle = angle * (180/pi)
        x = floor $ spriteState^.spriteStatePosition.pointX + 400 - fromIntegral px
        y = floor $ spriteState^.spriteStatePosition.pointY + 400 - fromIntegral py
        texture = sprite ^. spriteTexture
        renderRect = SDL.Rectangle (SDL.P $ V2 x y) (V2 w h)
    SDL.copyEx
        renderer texture Nothing (Just $ renderRect) (CDouble degAngle) pivot (V2 False False)
