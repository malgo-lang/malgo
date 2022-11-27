{-# LANGUAGE DeriveAnyClass #-}

module Malgo.Build where

import Control.Lens
import Data.Aeson (FromJSON, decodeFileStrict)
import Data.Graph (graphFromEdges, reverseTopSort)
import Data.List ((\\))
import Data.List qualified as List
import Koriel.MonadUniq (UniqSupply (UniqSupply))
import Malgo.Driver qualified as Driver
import Malgo.Parser (parseMalgo)
import Malgo.Prelude
import Malgo.Syntax (Decl (..), Module (..), ParsedDefinitions (..), _moduleName)
import Relude.Unsafe qualified as Unsafe
import System.Directory (getCurrentDirectory, makeAbsolute)
import System.FilePath (takeBaseName, (</>))
import System.FilePath.Glob (glob)

data Config = Config
  { sourceDirectories :: [FilePath],
    excludePatterns :: [FilePath]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

getWorkspaceDir :: IO FilePath
getWorkspaceDir = do
  pwd <- getCurrentDirectory
  return $ pwd </> ".malgo-work"

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
  sourceContents <- map (decodeUtf8 @Text) <$> traverse readFileBS sourceFiles'
  let parsedAstList = mconcat $ zipWith parse sourceFiles' sourceContents
  let moduleDepends = map takeImports parsedAstList
  let (graph, nodeFromVertex, _) = graphFromEdges moduleDepends
  let topSorted = map (nodeFromVertex >>> view _1) $ reverseTopSort graph

  _uniqSupply <- UniqSupply <$> newIORef 0
  _interfaces <- newIORef mempty
  _indexes <- newIORef mempty
  let compileOptions =
        map
          ( \path ->
              ( path,
                MalgoEnv
                  { _uniqSupply = _uniqSupply,
                    _interfaces = _interfaces,
                    _indexes = _indexes,
                    dstPath = workspaceDir </> "build" </> (takeBaseName path <> ".ll"),
                    compileMode = LLVM,
                    noOptimize = False,
                    lambdaLift = False,
                    inlineSize = 15,
                    debugMode = False,
                    _modulePaths = [workspaceDir </> "build"]
                  }
              )
          )
          topSorted
  for_ compileOptions \(path, env) -> do
    putStrLn ("Compile " <> path)
    Driver.compileFromAST path env (Unsafe.fromJust $ List.lookup path parsedAstList)
  where
    parse sourceFile sourceContent = case parseMalgo sourceFile sourceContent of
      Left _ -> []
      Right ast -> [(sourceFile, ast)]
    takeImports (sourceFile, Module {..}) =
      let ParsedDefinitions ds = _moduleDefinition
       in ( sourceFile,
            _moduleName,
            mapMaybe
              ( \case
                  Import _ imported _ -> Just imported
                  _ -> Nothing
              )
              ds
          )
