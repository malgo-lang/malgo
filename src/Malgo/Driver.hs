module Malgo.Driver (compile, compileFromAST) where

import Control.Lens (over, view, (^.))
import Control.Monad.Catch (catch)
import Koriel.Core.CodeGen (codeGen)
import Koriel.Core.Flat (flat)
import Koriel.Core.LambdaLift (lambdalift)
import Koriel.Core.Lint (lintProgram, runLint)
import Koriel.Core.Optimize (optimizeProgram)
import Koriel.Core.Syntax
import Koriel.MonadUniq
import Koriel.Pretty
import Malgo.Core.MlgToCore (mlgToCore)
import Malgo.Desugar.Pass (desugar)
import Malgo.Interface (buildInterface, dependencieList, loadInterface, storeInterface)
import Malgo.Parser (parseMalgo)
import Malgo.Prelude
import Malgo.Refine.Pass (refine)
import Malgo.Rename.Pass (rename)
import qualified Malgo.Rename.RnEnv as RnEnv
import qualified Malgo.Syntax as Syntax
import Malgo.Syntax.Extension
import qualified Malgo.TypeCheck.Pass as TypeCheck
import System.IO
  ( hPrint,
    hPutStrLn,
  )
import Text.Megaparsec
  ( errorBundlePretty,
  )

-- |
-- dumpHoge系のフラグによるダンプ出力を行うコンビネータ
--
-- 引数 m のアクションの返り値をpPrintしてstderrに吐く
withDump ::
  (MonadIO m, Pretty a) =>
  -- | dumpHoge系のフラグの値
  Bool ->
  String ->
  m a ->
  m a
withDump isDump label m = do
  result <- m
  when isDump $ liftIO do
    hPutStrLn stderr label
    hPrint stderr $ pPrint result
  pure result

compileFromAST :: Syntax.Module (Malgo 'Parse) -> Opt -> IO ()
compileFromAST parsedAst opt = runMalgoM ?? opt $ do
  uniqSupply <- view uniqSupply
  when (dumpParsed opt) $ liftIO do
    hPutStrLn stderr "=== PARSED ==="
    hPrint stderr $ pPrint parsedAst
  rnEnv <- RnEnv.genBuiltinRnEnv =<< ask
  (renamedAst, rnState) <- withDump (dumpRenamed opt) "=== RENAME ===" $ rename rnEnv parsedAst
  (typedAst, tcEnv) <- withDump (dumpTyped opt) "=== TYPE CHECK ===" $ TypeCheck.typeCheck rnEnv renamedAst
  refinedAst <- withDump (dumpRefine opt) "=== REFINE ===" $ refine tcEnv typedAst

  -- MlgToCore
  catch
    ( do
        mlgCore <- mlgToCore tcEnv refinedAst
        when (debugMode opt) do
          liftIO $ hPrint stderr $ pPrint mlgCore
    )
    (\e -> liftIO $ hPutStrLn stderr $ "MlgToCore Fail: " <> displayException (e :: SomeException))

  depList <- dependencieList (Syntax._moduleName typedAst) (rnState ^. RnEnv.dependencies)
  (dsEnv, core) <- withDump (dumpDesugar opt) "=== DESUGAR ===" $ desugar rnEnv tcEnv depList refinedAst
  let inf = buildInterface rnState dsEnv
  storeInterface inf
  when (debugMode opt) $ do
    inf <- loadInterface (Syntax._moduleName typedAst)
    liftIO $ do
      hPutStrLn stderr "=== INTERFACE ==="
      hPutStrLn stderr $ renderStyle (style {lineLength = 120}) $ pPrint inf
  runLint $ lintProgram core
  coreOpt <- if noOptimize opt then pure core else optimizeProgram uniqSupply (inlineSize opt) core
  when (dumpDesugar opt && not (noOptimize opt)) $
    liftIO $ do
      hPutStrLn stderr "=== OPTIMIZE ==="
      hPrint stderr $ pPrint $ over appProgram flat coreOpt
  runLint $ lintProgram coreOpt
  coreLL <- if noLambdaLift opt then pure coreOpt else lambdalift uniqSupply coreOpt
  when (dumpDesugar opt && not (noLambdaLift opt)) $
    liftIO $ do
      hPutStrLn stderr "=== LAMBDALIFT ==="
      hPrint stderr $ pPrint $ over appProgram flat coreLL
  coreLLOpt <- if noOptimize opt then pure coreLL else optimizeProgram uniqSupply (inlineSize opt) coreLL
  when (dumpDesugar opt && not (noLambdaLift opt) && not (noOptimize opt)) $
    liftIO $ do
      hPutStrLn stderr "=== LAMBDALIFT OPTIMIZE ==="
      hPrint stderr $ pPrint $ over appProgram flat coreLLOpt
  codeGen (srcName opt) (dstName opt) uniqSupply (Syntax._moduleName typedAst) coreLLOpt

-- | .mlgから.llへのコンパイル
compile :: Opt -> IO ()
compile opt = do
  src <- readFileText (srcName opt)
  parsedAst <- case parseMalgo (srcName opt) src of
    Right x -> pure x
    Left err -> error $ toText $ errorBundlePretty err
  when (dumpParsed opt) $ do
    hPutStrLn stderr "=== PARSE ==="
    hPrint stderr $ pPrint parsedAst
  compileFromAST parsedAst opt
