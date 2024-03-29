{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Malgo.Lsp.Index
  ( SymbolKind (..),
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

import Control.Concurrent.MVar (MVar)
import Control.Lens.TH
import Data.ByteString qualified as BS
import Data.HashMap.Strict qualified as HashMap
import Data.Store (Store, decodeEx, encode)
import Data.Store.TH
import Effectful
import Effectful.Reader.Static
import Effectful.State.Static.Local
import Generic.Data (Generically (..))
import Koriel.Id (ModuleName (..))
import Koriel.Pretty
import Malgo.Infer.TypeRep (Scheme, Type)
import Malgo.Interface (ModulePathList (..))
import Malgo.Prelude
import Malgo.Syntax.Extension (RnId)
import System.Directory qualified as Directory
import System.FilePath (takeFileName, (-<.>), (</>))
import Text.Megaparsec.Pos (Pos, SourcePos (..))

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
  { name :: Text,
    typeSignature :: Scheme Type,
    definitions :: [Range]
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

makeStore ''Info

instance Pretty Info where
  pretty Info {..} = pretty name <+> ":" <+> pretty typeSignature <+> pretty definitions

data Index = Index
  { references :: HashMap Info [Range],
    _definitionMap :: HashMap RnId Info,
    _symbolInfo :: HashMap RnId Symbol
  }
  deriving stock (Show, Generic)
  deriving (Monoid, Semigroup) via (Generically Index)
  deriving (Pretty) via (PrettyShow Index)

makeStore ''Index
makeFieldsNoPrefix ''Index

data LspOpt = LspOpt
  { modulePaths :: ModulePathList,
    indexes :: MVar (HashMap ModuleName Index)
  }

makeFieldsNoPrefix ''LspOpt

-- | 'findReferences' finds all references that are corresponding to the given 'SourcePos'.
-- It ignores file names.
findReferences :: SourcePos -> Index -> [Info]
findReferences pos (Index refs _ _) =
  HashMap.keys
    $ HashMap.filter (any (isInRange pos)) refs

isInRange :: SourcePos -> Range -> Bool
isInRange pos Range {_start, _end}
  | takeFileName (sourceName pos) == takeFileName (sourceName _start) = posToTuple _start <= posToTuple pos && posToTuple pos < posToTuple _end
  | otherwise = False
  where
    posToTuple :: SourcePos -> (Pos, Pos)
    posToTuple SourcePos {sourceLine, sourceColumn} = (sourceLine, sourceColumn)

-- | 'storeIndex' stores the given 'Index' to @dstPath -<.> "idx"@.
-- It only be used in 'MalgoM' monad, but importing 'Malgo.Monad' causes cyclic dependency.
storeIndex :: (Store a, MonadIO m) => FilePath -> a -> m ()
storeIndex dstPath index = do
  let encoded = encode index
  liftIO $ BS.writeFile (dstPath -<.> "idx") encoded

loadIndex :: (State (HashMap ModuleName Index) :> es, IOE :> es, Reader ModulePathList :> es) => ModuleName -> Eff es (Maybe Index)
loadIndex modName = do
  ModulePathList modulePaths <- ask
  indexes <- get @(HashMap ModuleName Index)
  case HashMap.lookup modName indexes of
    Just index -> pure $ Just index
    Nothing -> do
      message <- findAndReadFile modulePaths (convertString modName.raw <> ".idx")
      case message of
        Right x -> do
          modify @(HashMap ModuleName Index) $ HashMap.insert modName x
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
          idx <- liftIO $ decodeEx <$> BS.readFile (modPath </> modFile)
          pure $ Right idx
        else findAndReadFile rest modFile
