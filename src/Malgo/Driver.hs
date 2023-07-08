-- | Malgo.Driver is the entry point of `malgo to-ll`.
module Malgo.Driver (compile, compileFromAST, withDump) where

import Control.Exception (IOException, assert, catch)
import Data.Binary qualified as Binary
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.HashMap.Strict qualified as HashMap
import Data.String.Conversions.Monomorphic (toString)
import Data.Text.IO qualified as Text
import Error.Diagnose (addFile, prettyDiagnostic)
import Error.Diagnose.Compat.Megaparsec
import Koriel.Core.CodeGen.LLVM qualified as LLVM
import Koriel.Core.Flat qualified as Flat
import Koriel.Core.LambdaLift (lambdalift)
import Koriel.Core.Lint (lint)
import Koriel.Core.Optimize (OptimizeEnv (..), optimizeProgram)
import Koriel.Id (Id (Id, moduleName, name, sort), IdSort (External), ModuleName (..))
import Koriel.Pretty
import Malgo.Desugar.DsState (_nameEnv)
import Malgo.Desugar.Pass (desugar)
import Malgo.Infer.Pass qualified as Infer
import Malgo.Interface (buildInterface, loadInterface, toInterfacePath)
import Malgo.Link qualified as Link
import Malgo.Lsp.Index (storeIndex)
import Malgo.Lsp.Pass qualified as Lsp
import Malgo.Monad
import Malgo.Parser (parseMalgo)
import Malgo.Prelude
import Malgo.Refine.Pass (refine)
import Malgo.Rename.Pass (rename)
import Malgo.Rename.RnEnv qualified as RnEnv
import Malgo.Syntax qualified as Syntax
import Malgo.Syntax.Extension
import Prettyprinter qualified as PrettyPrinter
import Prettyprinter.Render.Text qualified as PrettyPrinter
import System.Exit (exitFailure)
import System.FilePath (takeBaseName, takeDirectory, (-<.>))
import System.IO (hFlush)

-- | `withDump` is the wrapper for check `dump` flag and output dump if that flag is `True`.
withDump ::
  (MonadIO m, Pretty a) =>
  -- | `dump` flag.
  Bool ->
  -- | Header of dump.
  String ->
  -- | The pass (e.g. `Malgo.Rename.Pass.rename rnEnv parsedAst`)
  m a ->
  m a
withDump isDump label m = do
  result <- m
  when isDump do
    hPutStrLn stderr label
    hPrint stderr $ pPrint result
  pure result

-- | Compile the parsed AST.
compileFromAST :: FilePath -> MalgoEnv -> Syntax.Module (Malgo 'Parse) -> IO ()
compileFromAST srcPath env parsedAst =
  runMalgoM env act
    `catch` \(e :: IOException) -> do
      hPutStrLn stderr $ "IO Exception: " <> show e
      exitFailure
  where
    moduleName = parsedAst.moduleName
    act = do
      when (convertString (takeBaseName srcPath) /= moduleName.raw)
        $ error "Module name must be source file's base name."

      uniqSupply <- asks (.uniqSupply)
      when env.debugMode do
        hPutStrLn stderr "=== PARSED ==="
        hPrint stderr $ pPrint parsedAst
      rnEnv <- RnEnv.genBuiltinRnEnv moduleName
      (renamedAst, rnState) <- withDump env.debugMode "=== RENAME ===" $ rename rnEnv parsedAst
      (typedAst, tcEnv) <- Infer.infer rnEnv renamedAst
      _ <- withDump env.debugMode "=== TYPE CHECK ===" $ pure typedAst
      refinedAst <- withDump env.debugMode "=== REFINE ===" $ refine tcEnv typedAst

      -- index <- withDump env.debugMode "=== INDEX ===" $ Lsp.index tcEnv refinedAst
      index <- Lsp.index tcEnv refinedAst
      storeIndex index

      (dsEnv, core) <- desugar tcEnv refinedAst

      core <- do
        core <- Flat.normalize core
        _ <- withDump env.debugMode "=== DESUGAR ===" $ pure core
        liftIO $ LBS.writeFile (env.dstPath -<.> "kor.bin") $ Binary.encode core

        let inf = buildInterface moduleName rnState dsEnv
        liftIO $ LBS.writeFile (toInterfacePath env.dstPath) $ Binary.encode inf

        -- check module paths include dstName's directory
        assert (takeDirectory env.dstPath `elem` env.modulePaths) pass
        core <- Link.link inf core
        liftIO $ writeFile (env.dstPath -<.> "kor") $ render $ pPrint core

        lint True core
        pure core

      when env.debugMode
        $ liftIO do
          hPutStrLn stderr "=== LINKED ==="
          hPrint stderr $ pPrint core

      when env.debugMode do
        inf <- loadInterface moduleName
        hPutStrLn stderr "=== INTERFACE ==="
        hPutStrLn stderr $ renderStyle (style {lineLength = 120}) $ pPrint inf

      coreOpt <-
        if env.noOptimize
          then pure core
          else
            optimizeProgram
              OptimizeEnv
                { uniqSupply,
                  moduleName,
                  debugMode = env.debugMode,
                  option = env.optimizeOption
                }
              core
              >>= Flat.normalize
      when (env.debugMode && not env.noOptimize) do
        hPutStrLn stderr "=== OPTIMIZE ==="
        hPrint stderr $ pPrint coreOpt
      when env.testMode do
        liftIO $ writeFile (env.dstPath -<.> "kor.opt") $ render $ pPrint coreOpt
      lint True coreOpt

      coreLL <- if env.lambdaLift then lambdalift uniqSupply moduleName coreOpt >>= Flat.normalize else pure coreOpt
      when (env.debugMode && env.lambdaLift)
        $ liftIO do
          hPutStrLn stderr "=== LAMBDALIFT ==="
          hPrint stderr $ pPrint coreLL
      when env.testMode do
        liftIO $ writeFile (env.dstPath -<.> "kor.opt.lift") $ render $ pPrint coreLL
      lint True coreLL

      -- Optimization after lambda lifting causes code explosion.
      -- The effect of lambda lifting is expected to be fully realized by backend's optimization.
      -- TODO: Can we only optimize the code that has been lambda lifted?
      -- TODO: Improve the function inliner to reduce the code explosion.
      -- TODO: Add more information to `call` instruction to improve the function inliner.
      -- Or simply skip inlining and do other optimizations.
      coreLLOpt <- if not env.noOptimize && env.lambdaLift then optimizeProgram OptimizeEnv {uniqSupply, moduleName, debugMode = env.debugMode, option = env.optimizeOption} coreLL else pure coreLL
      when (env.debugMode && env.lambdaLift && not env.noOptimize)
        $ liftIO do
          hPutStrLn stderr "=== OPTIMIZE AFTER LAMBDALIFT ==="
          hPrint stderr $ pPrint coreLLOpt
      when env.testMode do
        liftIO $ writeFile (env.dstPath -<.> "kor.opt.lift.opt") $ render $ pPrint coreLLOpt
      lint True coreLLOpt

      LLVM.codeGen srcPath env.dstPath uniqSupply moduleName (searchMain $ HashMap.toList dsEnv._nameEnv) coreLLOpt
    -- エントリーポイントとなるmain関数を検索する
    searchMain :: [(Id a, Id b)] -> Maybe (Id b)
    searchMain ((griffId@Id {sort = Koriel.Id.External}, coreId) : _)
      | griffId.name
          == "main"
          && griffId.moduleName
          == moduleName =
          Just coreId
    searchMain (_ : xs) = searchMain xs
    searchMain _ = Nothing

-- | Read the source file and parse it, then compile.
compile :: FilePath -> MalgoEnv -> IO ()
compile srcPath env = do
  src <- Text.readFile srcPath
  parsedAst <- case parseMalgo srcPath src of
    Right x -> pure x
    Left err ->
      let diag = errorDiagnosticFromBundle @Text Nothing "Parse error on input" Nothing err
          diag' = addFile diag srcPath (toString src)
       in do
            let message =
                  prettyDiagnostic True 4 diag'
                    & ( \x ->
                          if env.testMode
                            then PrettyPrinter.unAnnotate x
                            else x
                      )
                    & PrettyPrinter.layoutPretty PrettyPrinter.defaultLayoutOptions
                    & PrettyPrinter.renderStrict
                    & convertString
            BS.hPutStr stderr message -- ByteString.hPutStr is an atomic operation.
            hFlush stderr
            exitFailure
  when env.debugMode do
    hPutStrLn stderr "=== PARSE ==="
    hPrint stderr $ pPrint parsedAst
  compileFromAST srcPath env {Malgo.Monad.moduleName = parsedAst.moduleName} parsedAst
