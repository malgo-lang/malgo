{-# LANGUAGE DeriveAnyClass #-}

module Malgo.Build (run) where

import Control.Concurrent (getNumCapabilities)
import Control.Lens (Field2 (_2), view)
import Data.Aeson (FromJSON, decodeFileStrict)
import Data.List ((\\))
import Data.List qualified as List
import Data.List.Extra (chunksOf)
import Data.Maybe (fromJust)
import Koriel.Id (ModuleName (..))
import Koriel.MonadUniq (UniqSupply (UniqSupply))
import Malgo.Driver qualified as Driver
import Malgo.Monad (getWorkspaceDir, newMalgoEnv)
import Malgo.Parser (parseMalgo)
import Malgo.Prelude
import Malgo.Syntax (Decl (..), Module (..), ParsedDefinitions (..), _moduleName)
import System.Directory (getCurrentDirectory, makeAbsolute)
import System.FilePath ((</>))
import System.FilePath.Glob (glob)
import UnliftIO (mapConcurrently_)

data Config = Config
  { sourceDirectories :: [FilePath],
    excludePatterns :: [FilePath]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

readBuildConfig :: IO Config
readBuildConfig = do
  pwd <- getCurrentDirectory
  let configPath = pwd </> "build.json"
  mconfig <- decodeFileStrict configPath
  case mconfig of
    Nothing -> error $ "Failed to read build.json: " <> show configPath
    Just config -> pure config

getSourceDirs :: IO [FilePath]
getSourceDirs = do
  config <- readBuildConfig
  return config.sourceDirectories

getExcludePatterns :: IO [FilePath]
getExcludePatterns = do
  config <- readBuildConfig
  return config.excludePatterns

run :: IO ()
run = do
  putStrLn "Building..."
  workspaceDir <- getWorkspaceDir
  putStrLn $ "Workspace dir: " <> workspaceDir
  sourceDirs <- getSourceDirs
  sourceFiles <- concat <$> traverse (glob . (</> "**/*.mlg")) sourceDirs
  excludePatterns <- getExcludePatterns
  excludeFiles <- concat <$> traverse glob excludePatterns
  sourceFiles' <- traverse makeAbsolute $ sourceFiles \\ excludeFiles
  sourceContents <- map convertString <$> traverse readFile sourceFiles'
  let parsedAstList = mconcat $ zipWith parse sourceFiles' sourceContents
  let moduleDepends = map takeImports parsedAstList
  n <- getNumCapabilities
  let splited = split n $ map (\(_, i, o) -> (i, o)) moduleDepends

  _uniqSupply <- UniqSupply <$> newIORef 0
  _interfaces <- newIORef mempty
  _indexes <- newIORef mempty
  traverse_
    ( mapConcurrently_
        ( \(path, moduleName, _) -> do
            let ast = fromJust $ List.lookup path parsedAstList
            putStrLn ("Compile " <> path)
            env <- newMalgoEnv path [] (Just _uniqSupply) moduleName (Just _interfaces) (Just _indexes)
            Driver.compileFromAST path env ast
        )
        . mapMaybe (\mod -> List.find (view _2 >>> (== mod)) moduleDepends)
    )
    splited
  where
    parse sourceFile sourceContent = case parseMalgo sourceFile sourceContent of
      Left _ -> []
      Right ast -> [(sourceFile, ast)]
    takeImports (sourceFile, Module {..}) =
      let ParsedDefinitions ds = _moduleDefinition
       in ( sourceFile,
            _moduleName,
            ordNub $
              mapMaybe
                ( \case
                    Import _ imported _ -> Just imported
                    _ -> Nothing
                )
                ds
          )

split :: Int -> [(ModuleName, [ModuleName])] -> [[ModuleName]]
split n graph =
  reverse $ cut graph []
  where
    root :: (ModuleName, [ModuleName]) -> Maybe ModuleName
    root (x, []) = Just x
    root _ = Nothing
    cut :: [(ModuleName, [ModuleName])] -> [[ModuleName]] -> [[ModuleName]]
    cut [] acc = acc
    cut xs acc =
      let roots = mapMaybe root xs
          rests = filter (\(x, _) -> x `notElem` roots) xs
          roots' = chunksOf n roots
       in cut (map (second (\\ roots)) rests) (roots' <> acc)