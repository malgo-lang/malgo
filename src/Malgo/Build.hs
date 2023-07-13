{-# LANGUAGE DeriveAnyClass #-}

module Malgo.Build (run) where

import Control.Concurrent (getNumCapabilities)
import Control.Lens (Field2 (_2), view)
import Data.Aeson (FromJSON, decodeFileStrict)
import Data.ByteString.Lazy qualified as BL
import Data.List ((\\))
import Data.List qualified as List
import Data.List.Extra (chunksOf)
import Data.Maybe qualified as Maybe
import Effectful
import Effectful.Fail
import Effectful.Reader.Static
import Effectful.State.Static.Shared (evalState)
import Koriel.Core.Optimize (defaultOptimizeOption)
import Koriel.Id (ModuleName (..))
import Koriel.MonadUniq
import Malgo.Driver qualified as Driver
import Malgo.Interface (Interface)
import Malgo.Lsp.Index (Index)
import Malgo.Monad (CompileMode (..), Flag (..), getWorkspaceDir, runMalgoM)
import Malgo.Parser (parseMalgo)
import Malgo.Prelude
import Malgo.Syntax (Decl (..), Module (..), ParsedDefinitions (..))
import System.Directory (getCurrentDirectory, makeAbsolute)
import System.FilePath (takeBaseName, (</>))
import System.FilePath.Glob (glob)
import UnliftIO (mapConcurrently_)
import Witherable (ordNub)

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
  sourceContents <- map convertString <$> traverse BL.readFile sourceFiles'
  let parsedAstList = mconcat $ zipWith parse sourceFiles' sourceContents
  let moduleDepends = map takeImports parsedAstList
  n <- getNumCapabilities
  let splited = split n $ map (\(_, i, o) -> (i, o)) moduleDepends

  runEff
    $ runFailIO
    $ evalState @(HashMap ModuleName Interface) mempty
    $ evalState @(HashMap ModuleName Index) mempty
    $ withUnliftStrategy (ConcUnlift Persistent (Limited n))
    $ traverse_
      ( mapConcurrently_
          ( \(path, moduleName, _) -> do
              let ast = Maybe.fromJust $ List.lookup path parsedAstList
              liftIO $ putStrLn ("Compile " <> path)
              evalState (Uniq 0)
                $ runReader moduleName
                $ runMalgoM
                  (workspaceDir </> "build" </> takeBaseName path <> ".ll")
                  []
                  LLVM
                  Flag
                    { noOptimize = False,
                      lambdaLift = False,
                      debugMode = False,
                      testMode = False
                    }
                  defaultOptimizeOption
                $ Driver.compileFromAST path ast
          )
          . mapMaybe (\mod -> List.find (view _2 >>> (== mod)) moduleDepends)
      )
      splited
  where
    parse sourceFile sourceContent = case parseMalgo sourceFile sourceContent of
      Left _ -> []
      Right ast -> [(sourceFile, ast)]
    takeImports (sourceFile, Module {..}) =
      let ParsedDefinitions ds = moduleDefinition
       in ( sourceFile,
            moduleName,
            ordNub
              $ mapMaybe
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
