module Malgo.Build where

import Control.Lens
import Data.Graph (graphFromEdges, reverseTopSort)
import Data.List ((\\))
import Data.List qualified as List
import Dhall hiding (map)
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

instance FromDhall Config

getWorkspaceDir :: IO FilePath
getWorkspaceDir = do
  pwd <- getCurrentDirectory
  return $ pwd </> ".malgo-work"

getSourceDirs :: IO [FilePath]
getSourceDirs = do
  pwd <- getCurrentDirectory
  config <- input @Config auto (toText $ pwd </> "build.dhall")
  return config.sourceDirectories

getExcludePatterns :: IO [FilePath]
getExcludePatterns = do
  pwd <- getCurrentDirectory
  config <- input @Config auto (toText $ pwd </> "build.dhall")
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
                    _srcPath = path,
                    _dstPath = workspaceDir </> "build" </> (takeBaseName path <> ".ll"),
                    _compileMode = LLVM,
                    _dumpParsed = False,
                    _dumpRenamed = False,
                    _dumpTyped = False,
                    _dumpRefine = False,
                    _dumpDesugar = False,
                    _noOptimize = False,
                    lambdaLift = True,
                    _inlineSize = 10,
                    _debugMode = False,
                    _modulePaths = [workspaceDir </> "build"]
                  }
              )
          )
          topSorted
  for_ compileOptions \(path, env) -> do
    putStrLn ("Compile " <> env._srcPath)
    Driver.compileFromAST (Unsafe.fromJust $ List.lookup path parsedAstList) env
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
