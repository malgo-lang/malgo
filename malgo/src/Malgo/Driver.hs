module Malgo.Driver (compile, compileFromAST) where

import Data.Maybe (fromJust)
import qualified Data.Text.IO as T
import Data.Text.Prettyprint.Doc.Render.String (renderString)
import Koriel.Core.CodeGen (codeGen)
import Koriel.Core.Flat (flat)
import Koriel.Core.LambdaLift (lambdalift)
import Koriel.Core.Lint (lintProgram, runLint)
import Koriel.Core.Optimize (optimizeProgram)
import Koriel.Core.Syntax
import Koriel.MonadUniq
import Koriel.Pretty
import Malgo.Desugar.Pass (desugar)
import Malgo.Interface (buildInterface, loadInterface, storeInterface)
import Malgo.Parser (parseMalgo)
import Malgo.Prelude
import Malgo.Refine.Pass (refine)
import Malgo.Rename.Pass (rename)
import qualified Malgo.Rename.RnEnv as RnEnv
import qualified Malgo.Syntax as Syntax
import Malgo.Syntax.Extension
import qualified Malgo.TypeCheck.Pass as TypeCheck
import qualified Malgo.TypeCheck.TcEnv as TcEnv
import qualified Malgo.TypeRep.Static as Static
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
-- 引数 m のアクションの返り値をprettyしてstderrに吐く
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
    hPrint stderr $ pretty result
  pure result

compileFromAST :: Syntax.Module (Malgo 'Parse) -> Opt -> IO ()
compileFromAST parsedAst opt = runMalgoM ?? opt $ do
  uniqSupply <- view uniqSupply
  when (dumpParsed opt) $ liftIO do
    hPutStrLn stderr "=== PARSED ==="
    hPrint stderr $ pretty parsedAst
  rnEnv <- RnEnv.genBuiltinRnEnv =<< ask
  (renamedAst, rnState) <- withDump (dumpRenamed opt) "=== RENAME ===" $ rename rnEnv parsedAst
  (typedAst, tcEnv) <- withDump (dumpTyped opt) "=== TYPE CHECK ===" $ TypeCheck.typeCheck rnEnv renamedAst
  refinedAst <- refine tcEnv typedAst
  let varEnv = fromJust $ traverse (traverse Static.safeToType) $ tcEnv ^. TcEnv.varEnv
  let typeEnv = fromJust $ traverse (traverse Static.safeToType) $ tcEnv ^. TcEnv.typeEnv
  let fieldEnv = fromJust $ traverse (traverse Static.safeToType) $ tcEnv ^. TcEnv.fieldEnv
  (dsEnv, core) <- withDump (dumpDesugar opt) "=== DESUGAR ===" $ desugar varEnv typeEnv fieldEnv (tcEnv ^. TcEnv.rnEnv) refinedAst
  let inf = buildInterface rnState dsEnv
  storeInterface inf
  when (debugMode opt) $ do
    inf <- loadInterface (Syntax._moduleName typedAst)
    liftIO $ do
      hPutStrLn stderr "=== INTERFACE ==="
      hPutStrLn stderr $ renderString $ layoutSmart defaultLayoutOptions $ pretty inf
  runLint $ lintProgram core
  coreOpt <- if noOptimize opt then pure core else optimizeProgram uniqSupply (inlineSize opt) core
  when (dumpDesugar opt && not (noOptimize opt)) $
    liftIO $ do
      hPutStrLn stderr "=== OPTIMIZE ==="
      hPrint stderr $ pretty $ over appProgram flat coreOpt
  runLint $ lintProgram coreOpt
  coreLL <- if noLambdaLift opt then pure coreOpt else lambdalift uniqSupply coreOpt
  when (dumpDesugar opt && not (noLambdaLift opt)) $
    liftIO $ do
      hPutStrLn stderr "=== LAMBDALIFT ==="
      hPrint stderr $ pretty $ over appProgram flat coreLL
  coreLLOpt <- if noOptimize opt then pure coreLL else optimizeProgram uniqSupply (inlineSize opt) coreLL
  when (dumpDesugar opt && not (noLambdaLift opt) && not (noOptimize opt)) $
    liftIO $ do
      hPutStrLn stderr "=== LAMBDALIFT OPTIMIZE ==="
      hPrint stderr $ pretty $ over appProgram flat coreLLOpt

  codeGen (srcName opt) (dstName opt) uniqSupply coreLLOpt

-- | .mlgから.llへのコンパイル
compile :: Opt -> IO ()
compile opt = do
  src <- T.readFile (srcName opt)
  parsedAst <- case parseMalgo (srcName opt) src of
    Right x -> pure x
    Left err -> error $ errorBundlePretty err
  when (dumpParsed opt) $ do
    hPutStrLn stderr "=== PARSE ==="
    hPrint stderr $ pretty parsedAst
  compileFromAST parsedAst opt
