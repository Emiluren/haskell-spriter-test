{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module SpriterTypes where

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Language.C.Inline.Cpp as C
import Language.C.Inline.Context (ctxTypesTable)
import Language.C.Types (TypeSpecifier(..))

import qualified Language.Haskell.TH as TH

data CSpriterModel
data CEntityInstance

spriterCtx :: C.Context
spriterCtx = mempty
    { ctxTypesTable = spriterTypeTable
    }

spriterTypeTable :: Map TypeSpecifier TH.TypeQ
spriterTypeTable = Map.fromList
    [ (TypeName "SpriterModel", [t| CSpriterModel |])
    , (TypeName "EntityInstance", [t| CEntityInstance |])
    ]
