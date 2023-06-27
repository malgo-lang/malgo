{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Malgo.Lsp.Index (
  SymbolKind (..),
  Symbol (..),
  Info (..),
  Index (..),
  definitionMap,
  LspOpt (..),
  HasSymbolInfo (..),
  findReferences,
  loadIndex,
  storeIndex,
)
where

import Control.Lens (view)
import Control.Lens.TH
import Data.Binary (Binary, decodeFile, encode)
import Data.HashMap.Strict qualified as HashMap
import Data.String.Conversions (convertString)
import GHC.Records (HasField)
import Generic.Data (Generically (..))
import Koriel.Id (ModuleName (..))
import Koriel.Lens (HasModulePaths (..))
import Koriel.Pretty
import Malgo.Infer.TypeRep (Scheme, Type)
import Malgo.Prelude
import Malgo.Syntax.Extension (RnId)
import System.Directory qualified as Directory
import System.FilePath (takeFileName, (-<.>), (</>))
import Text.Megaparsec.Pos (Pos, SourcePos (..))

data SymbolKind = Data | TypeParam | Constructor | Function | Variable
  deriving stock (Show, Generic)
  deriving anyclass (Binary)

data Symbol = Symbol {kind :: SymbolKind, name :: Text, range :: Range}
  deriving stock (Show, Generic)
  deriving anyclass (Binary)

-- | An 'Info' records
--  * Symbol name
--  * Type
--  * Definition
data Info = Info
  { name :: Text,
    typeSignature :: Scheme Type,
    definitions :: [Range]
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Binary, Hashable)

instance Pretty Info where
  pPrint Info {..} = pPrint name <+> ":" <+> pPrint typeSignature <+> pPrint definitions

data Index = Index
  { references :: HashMap Info [Range],
    _definitionMap :: HashMap RnId Info,
    _symbolInfo :: HashMap RnId Symbol
  }
  deriving stock (Show, Generic)
  deriving anyclass (Binary)
  deriving (Monoid, Semigroup) via (Generically Index)
  deriving (Pretty) via (PrettyShow Index)

makeFieldsNoPrefix ''Index

data LspOpt = LspOpt
  { modulePaths :: [FilePath],
    indexes :: IORef (HashMap ModuleName Index)
  }

makeFieldsNoPrefix ''LspOpt

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

-- | 'storeIndex' stores the given 'Index' to @dstPath -<.> "idx"@.
-- It only be used in 'MalgoM' monad, but importing 'Malgo.Monad' causes cyclic dependency.
storeIndex :: (MonadReader s m, MonadIO m, HasField "dstPath" s FilePath) => Index -> m ()
storeIndex index = do
  dstPath <- asks (.dstPath)
  let encoded = encode index
  writeFileLBS (dstPath -<.> "idx") encoded

loadIndex :: (MonadReader s m, MonadIO m, HasField "modulePaths" s [FilePath], HasField "indexes" s (IORef (HashMap ModuleName Index))) => ModuleName -> m (Maybe Index)
loadIndex modName = do
  modPaths <- asks (.modulePaths)
  indexesRef <- asks (.indexes)
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
          idx <- liftIO $ decodeFile (modPath </> modFile)
          pure $ Right idx
        else findAndReadFile rest modFile
