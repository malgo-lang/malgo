-- | Malgo.Driver is the entry point of `malgo to-ll`.
module Malgo.Driver (compile, withDump) where

import Control.Lens (over, view, (^.))
import Koriel.Core.CodeGen (codeGen)
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
import Malgo.Lsp.Pass qualified as Lsp
import Malgo.Parser (parseMalgo)
import Malgo.Prelude
import Malgo.Refine.Pass (refine)
import Malgo.Rename.Pass (rename)
import Malgo.Rename.RnEnv qualified as RnEnv
import Malgo.Syntax qualified as Syntax
import Malgo.Syntax.Extension
import Text.Megaparsec
  ( errorBundlePretty,
  )

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
compileFromAST :: Syntax.Module (Malgo 'Parse) -> ToLLOpt -> IO ()
compileFromAST parsedAst opt = runMalgoM ?? opt $ do
  uniqSupply <- view uniqSupply
  when (view dumpParsed opt) do
    hPutStrLn stderr "=== PARSED ==="
    hPrint stderr $ pPrint parsedAst
  rnEnv <- RnEnv.genBuiltinRnEnv (parsedAst._moduleName) =<< ask
  (renamedAst, rnState) <- withDump (view dumpRenamed opt) "=== RENAME ===" $ rename rnEnv parsedAst
  (typedAst, tcEnv) <- withDump (view dumpTyped opt) "=== TYPE CHECK ===" $ Infer.infer rnEnv renamedAst
  refinedAst <- withDump (view dumpRefine opt) "=== REFINE ===" $ refine tcEnv typedAst

  index <- withDump (view debugMode opt) "=== INDEX ===" $ Lsp.index tcEnv refinedAst

  depList <- dependencieList (typedAst._moduleName) (rnState ^. RnEnv.dependencies)
  (dsEnv, core) <- withDump (view dumpDesugar opt) "=== DESUGAR ===" $ desugar tcEnv depList refinedAst

  let inf = buildInterface rnState dsEnv index
  storeInterface inf
  when (view debugMode opt) $ do
    inf <- loadInterface (typedAst._moduleName)
    hPutStrLn stderr "=== INTERFACE ==="
    hPutStrLn stderr $ renderStyle (style {lineLength = 120}) $ pPrint inf

  lint core
  coreOpt <- if view noOptimize opt then pure core else optimizeProgram uniqSupply (view inlineSize opt) core
  when (view dumpDesugar opt && not (view noOptimize opt)) do
    hPutStrLn stderr "=== OPTIMIZE ==="
    hPrint stderr $ pPrint $ over appProgram flat coreOpt
  lint coreOpt
  coreLL <- if view noLambdaLift opt then pure coreOpt else lambdalift uniqSupply coreOpt
  when (view dumpDesugar opt && not (view noLambdaLift opt)) $
    liftIO $ do
      hPutStrLn stderr "=== LAMBDALIFT ==="
      hPrint stderr $ pPrint $ over appProgram flat coreLL
  coreLLOpt <- if view noOptimize opt then pure coreLL else optimizeProgram uniqSupply (view inlineSize opt) coreLL
  when (view dumpDesugar opt && not (view noLambdaLift opt) && not (view noOptimize opt)) $
    liftIO $ do
      hPutStrLn stderr "=== LAMBDALIFT OPTIMIZE ==="
      hPrint stderr $ pPrint $ over appProgram flat coreLLOpt
  codeGen (view srcName opt) (view dstName opt) uniqSupply (typedAst._moduleName) coreLLOpt

-- | Read the source file and parse it, then compile.
compile :: ToLLOpt -> IO ()
compile opt = do
  src <- decodeUtf8 <$> readFileBS (view srcName opt)
  parsedAst <- case parseMalgo (view srcName opt) src of
    Right x -> pure x
    Left err -> error $ toText $ errorBundlePretty err
  when (view dumpParsed opt) $ do
    hPutStrLn stderr "=== PARSE ==="
    hPrint stderr $ pPrint parsedAst
  compileFromAST parsedAst opt
