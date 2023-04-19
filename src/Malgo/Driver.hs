-- | Malgo.Driver is the entry point of `malgo to-ll`.
module Malgo.Driver (compile, compileFromAST, withDump) where

import Control.Exception (assert)
import Data.Binary qualified as Binary
import Data.String.Conversions (ConvertibleStrings (convertString))
import Error.Diagnose (addFile, defaultStyle, printDiagnostic)
import Error.Diagnose.Compat.Megaparsec
import Koriel.Core.CodeGen.LLVM (codeGen)
import Koriel.Core.CodeGen.PrintLLVM qualified as PrintLLVM
import Koriel.Core.Flat (flat)
import Koriel.Core.LambdaLift (lambdalift)
import Koriel.Core.Lint (lint)
import Koriel.Core.Optimize (optimizeProgram)
import Koriel.Id (ModuleName (..))
import Koriel.Pretty
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
import System.FilePath (takeBaseName, takeDirectory, (-<.>))

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
compileFromAST srcPath env parsedAst = runMalgoM env act
  where
    moduleName = parsedAst._moduleName
    act = do
      when (convertString (takeBaseName srcPath) /= moduleName.raw) $
        error "Module name must be source file's base name."

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
      core <- flat core
      _ <- withDump env.debugMode "=== DESUGAR ===" $ pure core
      writeFileLBS (env.dstPath -<.> "kor.bin") $ Binary.encode core

      let inf = buildInterface moduleName rnState dsEnv
      writeFileLBS (toInterfacePath env.dstPath) $ Binary.encode inf

      -- check module paths include dstName's directory
      assert (takeDirectory env.dstPath `elem` env._modulePaths) pass
      core <- Link.link inf core
      writeFile (env.dstPath -<.> "kor") $ render $ pPrint core

      lint core

      when env.debugMode $
        liftIO do
          hPutStrLn stderr "=== LINKED ==="
          hPrint stderr $ pPrint core

      when env.debugMode do
        inf <- loadInterface moduleName
        hPutStrLn stderr "=== INTERFACE ==="
        hPutStrLn stderr $ renderStyle (style {lineLength = 120}) $ pPrint inf

      coreOpt <- if env.noOptimize then pure core else optimizeProgram uniqSupply moduleName env.debugMode env.optimizeOption core
      when (env.debugMode && not env.noOptimize) do
        hPutStrLn stderr "=== OPTIMIZE ==="
        hPrint stderr $ pPrint coreOpt
        writeFile (env.dstPath -<.> "kor.opt") $ render $ pPrint coreOpt
      lint coreOpt
      coreLL <- if env.lambdaLift then lambdalift uniqSupply moduleName coreOpt else pure coreOpt
      lint coreLL
      when (env.debugMode && env.lambdaLift) $
        liftIO do
          hPutStrLn stderr "=== LAMBDALIFT ==="
          hPrint stderr $ pPrint coreLL
          writeFile (env.dstPath -<.> "kor.opt.lift") $ render $ pPrint coreLL

      case env.compileMode of
        LLVM -> do
          codeGen srcPath env moduleName dsEnv coreLL
        PrintLLVM -> do
          PrintLLVM.codeGen srcPath env moduleName coreLL

-- | Read the source file and parse it, then compile.
compile :: FilePath -> MalgoEnv -> IO ()
compile srcPath env = do
  src <- decodeUtf8 <$> readFileBS srcPath
  parsedAst <- case parseMalgo srcPath src of
    Right x -> pure x
    Left err ->
      let diag = errorDiagnosticFromBundle @Text Nothing "Parse error on input" Nothing err
          diag' = addFile diag srcPath (toString src)
       in printDiagnostic stderr True True 4 defaultStyle diag' >> exitFailure
  when env.debugMode do
    hPutStrLn stderr "=== PARSE ==="
    hPrint stderr $ pPrint parsedAst
  compileFromAST srcPath env {moduleName = parsedAst._moduleName} parsedAst
