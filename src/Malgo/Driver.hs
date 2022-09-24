-- | Malgo.Driver is the entry point of `malgo to-ll`.
module Malgo.Driver (compile, compileFromAST, withDump) where

import Control.Lens (over, view, (^.))
import Data.Store (encode)
import Data.String.Conversions (ConvertibleStrings (convertString))
import Error.Diagnose (addFile, defaultStyle, printDiagnostic)
import Error.Diagnose.Compat.Megaparsec
import Koriel.Core.CodeGen.LLVM (codeGen)
import Koriel.Core.CodeGen.Scheme qualified as Scheme
import Koriel.Core.Flat (flat)
import Koriel.Core.LambdaLift (lambdalift)
import Koriel.Core.Lint (lint)
import Koriel.Core.Optimize (optimizeProgram)
import Koriel.Core.Syntax
import Koriel.Lens
import Koriel.Pretty
import Malgo.Desugar.Pass (desugar)
import Malgo.Infer.Pass qualified as Infer
import Malgo.Interface (buildInterface, dependencieList, loadInterface, storeInterface)
import Malgo.Lsp.Index (storeIndex)
import Malgo.Lsp.Pass qualified as Lsp
import Malgo.Parser (parseMalgo)
import Malgo.Prelude
import Malgo.Refine.Pass (refine)
import Malgo.Rename.Pass (rename)
import Malgo.Rename.RnEnv qualified as RnEnv
import Malgo.Syntax qualified as Syntax
import Malgo.Syntax.Extension
import System.Directory (makeAbsolute)
import System.FilePath ((-<.>))

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
compileFromAST :: Syntax.Module (Malgo 'Parse) -> MalgoEnv -> IO ()
compileFromAST parsedAst env = runMalgoM env act
  where
    act = do
      uniqSupply <- view uniqSupply
      when (view dumpParsed env) do
        hPutStrLn stderr "=== PARSED ==="
        hPrint stderr $ pPrint parsedAst
      rnEnv <- RnEnv.genBuiltinRnEnv (parsedAst._moduleName) =<< ask
      (renamedAst, rnState) <- withDump (view dumpRenamed env) "=== RENAME ===" $ rename rnEnv parsedAst
      (typedAst, tcEnv) <- Infer.infer rnEnv renamedAst
      _ <- withDump (view dumpTyped env) "=== TYPE CHECK ===" $ pure typedAst
      refinedAst <- withDump (view dumpRefine env) "=== REFINE ===" $ refine tcEnv typedAst

      index <- withDump (view debugMode env) "=== INDEX ===" $ Lsp.index tcEnv refinedAst
      storeIndex index

      depList <- dependencieList (typedAst._moduleName) (rnState ^. RnEnv.dependencies)
      (dsEnv, core) <- desugar tcEnv depList refinedAst
      _ <- withDump (view dumpDesugar env) "=== DESUGAR ===" $ pure core

      let inf = buildInterface rnEnv._moduleName rnState dsEnv
      storeInterface inf
      when (view debugMode env) $ do
        inf <- loadInterface (typedAst._moduleName)
        hPutStrLn stderr "=== INTERFACE ==="
        hPutStrLn stderr $ renderStyle (style {lineLength = 120}) $ pPrint inf

      lint core
      coreOpt <- if view noOptimize env then pure core else optimizeProgram uniqSupply (view inlineSize env) core
      when (view dumpDesugar env && not (view noOptimize env)) do
        hPutStrLn stderr "=== OPTIMIZE ==="
        hPrint stderr $ pPrint $ over appProgram flat coreOpt
      lint coreOpt
      coreLL <- if view noLambdaLift env then pure coreOpt else lambdalift uniqSupply typedAst._moduleName coreOpt
      when (view dumpDesugar env && not (view noLambdaLift env)) $
        liftIO $ do
          hPutStrLn stderr "=== LAMBDALIFT ==="
          hPrint stderr $ pPrint $ over appProgram flat coreLL
      coreLLOpt <- if view noOptimize env then pure coreLL else optimizeProgram uniqSupply (view inlineSize env) coreLL
      when (view dumpDesugar env && not (view noLambdaLift env) && not (view noOptimize env)) $
        liftIO $ do
          hPutStrLn stderr "=== LAMBDALIFT OPTIMIZE ==="
          hPrint stderr $ pPrint $ over appProgram flat coreLLOpt
      writeFileBS (view dstName env -<.> "kor") $ encode coreLLOpt
      case view compileMode env of
        LLVM -> codeGen (view srcName env) (view dstName env) uniqSupply (typedAst._moduleName) coreLLOpt
        Scheme -> do
          code <- Scheme.codeGen uniqSupply coreLLOpt
          writeFileBS (view dstName env -<.> "scm") $ convertString $ render $ sep $ map pPrint code

-- | Read the source file and parse it, then compile.
compile :: MalgoEnv -> IO ()
compile env = do
  srcPath <- makeAbsolute $ view srcName env
  src <- decodeUtf8 <$> readFileBS srcPath
  parsedAst <- case parseMalgo srcPath src of
    Right x -> pure x
    Left err ->
      let diag = errorDiagnosticFromBundle @Text Nothing "Parse error on input" Nothing err
          diag' = addFile diag (view srcName env) (toString src)
       in printDiagnostic stderr True True 4 defaultStyle diag' >> exitFailure
  when (view dumpParsed env) $ do
    hPutStrLn stderr "=== PARSE ==="
    hPrint stderr $ pPrint parsedAst
  compileFromAST parsedAst env
