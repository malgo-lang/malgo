{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Malgo.Lsp.Index where

import Control.Lens (view)
import Control.Lens.TH
import Data.HashMap.Strict qualified as HashMap
import Data.Store (Store, encode)
import Data.Store qualified as Store
import Data.Store.TH (makeStore)
import Data.String.Conversions (convertString)
import GHC.Records (HasField)
import Koriel.Id (ModuleName (..))
import Koriel.Lens (HasModulePaths (modulePaths))
import Koriel.Pretty
import Malgo.Infer.TypeRep (Scheme, Type)
import Malgo.Prelude
import Malgo.Syntax.Extension (RnId)
import System.Directory qualified as Directory
import System.FilePath (takeFileName, (-<.>), (</>))
import Text.Megaparsec.Pos (Pos, SourcePos (..))
import Text.Pretty.Simple (pShow)

data SymbolKind = Data | TypeParam | Constructor | Function | Variable
  deriving stock (Show, Generic)

makeStore ''SymbolKind

data Symbol = Symbol {kind :: SymbolKind, name :: Text, range :: Range}
  deriving stock (Show, Generic)

makeStore ''Symbol

-- | An 'Info' records
--  * Symbol name
--  * Type
--  * Definition
data Info = Info
  { _name :: Text,
    typeSignature :: Scheme Type,
    definitions :: [Range]
  }
  deriving stock (Eq, Ord, Show, Generic)

instance Hashable Info

instance Pretty Info where
  pPrint Info {..} = pPrint _name <+> ":" <+> pPrint typeSignature <+> pPrint definitions

makeStore ''Info
makeFieldsNoPrefix ''Info

data Index = Index
  { _references :: HashMap Info [Range],
    _definitionMap :: HashMap RnId Info,
    _symbolInfo :: HashMap RnId Symbol
  }
  deriving stock (Show, Generic)

instance Semigroup Index where
  (Index refs1 defs1 syms1) <> (Index refs2 defs2 syms2) = Index (refs1 <> refs2) (defs1 <> defs2) (syms1 <> syms2)

instance Monoid Index where
  mempty = Index mempty mempty mempty

makeStore ''Index

instance Pretty Index where
  pPrint = text . toString . pShow

makeFieldsNoPrefix ''Index

-- | 'findReferences' finds all references that are corresponding to the given 'SourcePos'.
-- It ignores file names.
findReferences :: SourcePos -> Index -> [Info]
findReferences pos (Index refs _ _) =
  HashMap.keys $
    HashMap.filter (any (isInRange pos)) refs

isInRange :: SourcePos -> Range -> Bool
isInRange pos Range {_start, _end}
  | takeFileName (sourceName pos) == takeFileName (sourceName _start) = posToTuple _start <= posToTuple pos && posToTuple pos < posToTuple _end
  | otherwise = False
  where
    posToTuple :: SourcePos -> (Pos, Pos)
    posToTuple SourcePos {sourceLine, sourceColumn} = (sourceLine, sourceColumn)

storeIndex :: (MonadReader s m, Store a, MonadIO m, HasField "dstPath" s FilePath) => a -> m ()
storeIndex index = do
  dstPath <- asks (.dstPath)
  let encoded = encode index
  writeFileBS (dstPath -<.> "idx") encoded

loadIndex :: (MonadReader s m, MonadIO m, HasModulePaths s [FilePath], HasIndexes s (IORef (HashMap ModuleName Index))) => ModuleName -> m (Maybe Index)
loadIndex modName = do
  modPaths <- view modulePaths
  indexesRef <- view indexes
  indexes <- readIORef indexesRef
  case HashMap.lookup modName indexes of
    Just index -> pure $ Just index
    Nothing -> do
      message <- findAndReadFile modPaths (convertString modName.raw <> ".idx")
      case message of
        Right x -> do
          writeIORef indexesRef $ HashMap.insert modName x indexes
          pure $ Just x
        Left err -> do
          hPrint stderr err
          pure Nothing
  where
    findAndReadFile [] modFile = pure $ Left $ "Cannot find " <> modFile
    findAndReadFile (modPath : rest) modFile = do
      isExistModFile <- liftIO $ Directory.doesFileExist (modPath </> modFile)
      if isExistModFile
        then do
          raw <- readFileBS (modPath </> modFile)
          Right <$> liftIO (Store.decodeIO raw)
        else findAndReadFile rest modFile
