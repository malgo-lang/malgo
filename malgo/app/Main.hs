{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import qualified Data.List as List
import Development.Shake
import Development.Shake.FilePath
import Language.Malgo.Driver (compile)
import Language.Malgo.Prelude hiding ((<.>))
import System.Console.GetOpt
import System.Directory (XdgDirectory (XdgData), getXdgDirectory)
import System.Environment (getArgs)

data Flags
  = DumpParsed
  | DumpRenamed
  | DumpTyped
  | DumpDesugar
  | GenCoreJSON
  | NoOptimize
  | NoLambdaLift
  | InlineSize Int
  | ViaBinding
  | DebugMode
  | ModulePath FilePath
  | OutputPath FilePath
  deriving stock (Eq, Show)

makePrisms ''Flags

flags :: [OptDescr (Either a Flags)]
flags =
  [ Option "" ["dump-parsed"] (NoArg $ Right DumpParsed) "Dump parsed ast.",
    Option "" ["dump-renamed"] (NoArg $ Right DumpRenamed) "Dump renamed ast.",
    Option "" ["dump-typed"] (NoArg $ Right DumpTyped) "Dump typechecked ast.",
    Option "" ["dump-desugar"] (NoArg $ Right DumpDesugar) "Dump intermediate representation (Koriel.Core).",
    Option "" ["gen-core-json"] (NoArg $ Right GenCoreJSON) "Generate Koriel.Core json file.",
    Option "" ["no-optimize"] (NoArg $ Right NoOptimize) "Disable optimization.",
    Option "" ["no-lambdalift"] (NoArg $ Right NoLambdaLift) "Disable lambdalifting.",
    Option "" ["inline-size"] (OptArg (Right . InlineSize . maybe 10 read) "natural") "Maximum size of terms that can be inlined.",
    Option "" ["via-binding"] (NoArg $ Right ViaBinding) "Use C++'s LLVM API.",
    Option "" ["debug-mode"] (NoArg $ Right DebugMode) "Enable debug mode.",
    Option "M" ["module-path"] (ReqArg (Right . ModulePath) "file path") "Module search path.",
    Option "o" [] (ReqArg (Right . OutputPath) "file path") "Output file path."
  ]

data BuildOption = BuildOption
  { -- | working directory (default: .malgo-work/)
    workDir :: FilePath,
    -- | files that need to be compiled to .ll
    sourceFiles :: [FilePath],
    -- | \$XDG_DATA_HOME/malgo/rts.c
    runtimePath :: FilePath,
    malgoOpt :: Opt
  }
  deriving stock (Show)

data Dep = Dep FilePath String
  deriving stock (Eq, Show)

parseBuildOption :: [Flags] -> [String] -> Rules BuildOption
parseBuildOption flags args = do
  shakeOptions <- getShakeOptionsRules
  let workDir = shakeFiles shakeOptions
  runtimeDirectory <- liftIO $ getXdgDirectory XdgData "malgo"
  pure
    BuildOption
      { workDir = workDir,
        sourceFiles = filter ("mlg" `isExtensionOf`) args,
        runtimePath = runtimeDirectory </> "rts.c",
        malgoOpt =
          Opt
            { srcName = "",
              dstName = "",
              dumpParsed = DumpParsed `elem` flags,
              dumpRenamed = DumpRenamed `elem` flags,
              dumpTyped = DumpTyped `elem` flags,
              dumpDesugar = DumpDesugar `elem` flags,
              genCoreJSON = GenCoreJSON `elem` flags,
              noOptimize = NoOptimize `elem` flags,
              noLambdaLift = NoLambdaLift `elem` flags,
              inlineSize = fromMaybe 10 $ asumMap (preview _InlineSize) flags,
              viaBinding = ViaBinding `elem` flags,
              debugMode = DebugMode `elem` flags,
              modulePaths = mapMaybe (preview _ModulePath) flags
            }
      }

linkTo :: BuildOption -> FilePath -> Rules ()
linkTo buildOption output = do
  output %> \_ -> do
    -- .ll
    -- targetFilesとdepLLVMFilesは重複しうる
    let targetFiles = map (getTargetFile buildOption) (sourceFiles buildOption)
    let depLLVMFiles = map toLLVMFile $ concatMap (getDependencies buildOption) (sourceFiles buildOption)
    let needFiles = runtimePath buildOption : targetFiles <> depLLVMFiles
    need (runtimePath buildOption : targetFiles <> depLLVMFiles)
    cmd_ (concat $ ["clang $(pkg-config bdw-gc --libs --cflags) "] <> needFiles <> [" -o ", output])

simpleCompile :: BuildOption -> FilePath -> Rules ()
simpleCompile buildOption sourceFile = do
  let targetFile = getTargetFile buildOption sourceFile
  -- .mlgi
  let interfaceFile = getInterfaceFile buildOption sourceFile
  [targetFile, interfaceFile] |%> \_ -> do
    let dependencies = getDependencies buildOption sourceFile
    need (sourceFile : map toInterfaceFile dependencies)
    liftIO $ compile sourceFile targetFile (malgoOpt buildOption)

-- \$XDG_DATA_HOME/malgo/**/にある.mlgのうち、
-- モジュール名がsourceFilesと一致しないものをすべてコンパイルして
-- .malgo-work/build/以下におく。

-- $XDG_DATA_HOME/malgo/**/*.mlgのモジュール名が被ったらエラー
-- TODO: もっとリッチなモジュールシステム

compileInstalledModules :: BuildOption -> Rules ()
compileInstalledModules buildOption = error "not implemented"

-- .malgo-work/build/Hoge.ll
toLLVMFile :: Dep -> FilePath
toLLVMFile (Dep path name) = normaliseEx $ path </> name <.> "ll"

-- .malgo-work/build/Hoge.mlgi
toInterfaceFile :: Dep -> FilePath
toInterfaceFile (Dep path name) = normaliseEx $ path </> name <.> "mlgi"

-- [(.malgo-work/build, Hoge)]
getDependencies :: BuildOption -> FilePath -> [Dep]
getDependencies buildOption sourceFile = error "not implemented"

-- .malgo-work/build/Hoge.mlgi
getInterfaceFile :: BuildOption -> FilePath -> FilePath
getInterfaceFile buildOption sourceFile = workDir buildOption </> "build" </> takeFileName sourceFile -<.> "mlgi"

-- .malgo-work/build/Hoge.ll
getTargetFile :: BuildOption -> FilePath -> FilePath
getTargetFile buildOption sourceFile = workDir buildOption </> "build" </> takeFileName sourceFile -<.> "ll"

main :: IO ()
main = shakeArgsOptionsWith shakeOptions {shakeFiles = ".malgo-work"} flags $ \_ flags args -> do
  clArgs <- liftIO getArgs
  case clArgs of
    ("only-compile" : sourceFile : _) ->
      pure $ Just (shakeOptions', onlyCompileRule sourceFile flags args >> want ["only-compile"] >> addHelpSuffix helpMessage)
    _ -> pure Nothing
  where
    onlyCompileRule sourceFile flags args = do
      buildOption <- parseBuildOption flags args
      compileInstalledModules buildOption
      simpleCompile buildOption sourceFile
      phony "only-compile" do
        let targetFile = getTargetFile buildOption sourceFile
        need [targetFile]
    shakeOptions' = shakeOptions {shakeFiles = ".malgo-work"}

helpMessage :: String
helpMessage = "EMPTY"
