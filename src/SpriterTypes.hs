{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module SpriterTypes where

import Data.Map (Map)
import qualified Data.Map as Map

import Foreign.C.Types (CDouble)

import qualified Language.C.Inline.Cpp as C
import Language.C.Inline.Context (ctxTypesTable)
import Language.C.Types (TypeSpecifier(..))

import qualified Language.Haskell.TH as TH

import qualified SDL

data CSpriterModel
data CEntityInstance

spriterCtx :: C.Context
spriterCtx = mempty
    { ctxTypesTable = spriterTypeTable
    }

data Sprite = Sprite
    { _spriteTexture :: SDL.Texture
    , _spritePivotX :: CDouble
    , _spritePivotY :: CDouble
    , _spriteName :: String
    }

spriterTypeTable :: Map TypeSpecifier TH.TypeQ
spriterTypeTable = Map.fromList
    [ (TypeName "SpriterModel", [t| CSpriterModel |])
    , (TypeName "EntityInstance", [t| CEntityInstance |])
    , (TypeName "HaskellSprite", [t| Sprite |])
    ]
