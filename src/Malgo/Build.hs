module Malgo.Build where

import Control.Lens
import Data.Graph (graphFromEdges, reverseTopSort)
import Data.List ((\\))
import Dhall hiding (map)
import Malgo.Driver qualified as Driver
import Malgo.Parser (parseMalgo)
import Malgo.Prelude
import Malgo.Syntax (Decl (..), Module (..), ParsedDefinitions (..), _moduleName)
import System.Directory (getCurrentDirectory)
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
  let sourceFiles' = sourceFiles \\ excludeFiles
  sourceContents <- map (decodeUtf8 @Text) <$> traverse readFileBS sourceFiles'
  let parsedAstList = mconcat $ zipWith parse sourceFiles' sourceContents
  let moduleDepends = map takeImports parsedAstList
  let (graph, nodeFromVertex, _) = graphFromEdges moduleDepends
  let topSorted = map (nodeFromVertex >>> view _1) $ reverseTopSort graph
  let compileOptions =
        map
          ( \path ->
              (defaultToLLOpt path)
                { _dstName = workspaceDir </> "build" </> (takeBaseName path <> ".ll"),
                  _modulePaths = [workspaceDir </> "build"]
                }
          )
          topSorted
  for_ compileOptions \opt -> do
    putStrLn ("Compile " <> opt._srcName)
    Driver.compile opt
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