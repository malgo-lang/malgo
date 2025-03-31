-- | Malgo.Driver is the entry point of `malgo to-ll`.
module Malgo.Driver (compile, compileFromAST, withDump, failIfError) where

import Data.ByteString qualified as BS
import Effectful
import Effectful.Error.Static (CallStack, prettyCallStack, runError)
import Effectful.Reader.Static
import Effectful.State.Static.Local
import Malgo.Core.Eval (EvalError, defaultStderr, defaultStdin, defaultStdout, eval)
import Malgo.Core.Flat qualified as Flat
import Malgo.Core.LambdaLift (lambdalift)
import Malgo.Core.Lint (lint)
import Malgo.Core.Optimize (OptimizeOption, optimizeProgram)
import Malgo.Core.Syntax (Program)
import Malgo.Core.Type (Type)
import Malgo.Desugar.Pass (desugar)
import Malgo.Id (Meta (..))
import Malgo.Infer.Pass qualified as Infer
import Malgo.Interface (Interface, buildInterface, loadInterface)
import Malgo.Link qualified as Link
import Malgo.Module
import Malgo.Monad
import Malgo.MonadUniq
import Malgo.NewParser (parse)
import Malgo.NewRename.Pass (rename)
import Malgo.NewRename.RnEnv qualified as RnEnv
import Malgo.NewRename.RnState (RnState (..))
import Malgo.Prelude
import Malgo.Refine.Pass (refine)
import Malgo.Syntax qualified as Syntax
import Malgo.Syntax.Extension
import System.Exit (exitFailure)
import Text.Megaparsec (errorBundlePretty)

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
    hPrint stderr $ pretty result
  pure result

failIfError :: (Show e) => Either (CallStack, e) a -> a
failIfError = \case
  Left (callStack, err) -> error $ prettyCallStack callStack <> "\n" <> show err
  Right x -> x

-- | Compile the parsed AST to Core representation.
compileToCore ::
  ( Reader OptimizeOption :> es,
    Reader Flag :> es,
    IOE :> es,
    State (Map ModuleName Interface) :> es,
    State Uniq :> es,
    Workspace :> es
  ) =>
  ArtifactPath ->
  Syntax.Module (Malgo NewParse) ->
  Eff es (Program (Meta Type))
compileToCore srcPath parsedAst = do
  let moduleName = parsedAst.moduleName
  registerModule moduleName srcPath
  flags <- ask @Flag

  when flags.debugMode $ liftIO do
    hPutStrLn stderr "=== PARSED ==="
    hPrint stderr $ pretty parsedAst
  rnEnv <- RnEnv.genBuiltinRnEnv
  (renamedAst, rnState) <-
    withDump flags.debugMode "=== RENAME ===" $ failIfError <$> rename rnEnv parsedAst
  (typedAst, tcEnv) <- Infer.infer rnEnv renamedAst
  _ <- withDump flags.debugMode "=== TYPE CHECK ===" $ pure typedAst
  refinedAst <- withDump flags.debugMode "=== REFINE ===" $ refine tcEnv typedAst

  (dsEnv, core) <- desugar tcEnv refinedAst

  core <- do
    core <- runReader moduleName $ Flat.normalize core
    _ <- withDump flags.debugMode "=== DESUGAR ===" $ pure core
    save srcPath ".mo" (ViaStore core)

    let inf = buildInterface moduleName rnState tcEnv dsEnv
    save srcPath ".mlgi" (ViaStore inf)

    core <- Link.link inf core
    when flags.testMode do
      save srcPath ".kor" (ViaShow core)

    lint True core
    pure core

  when flags.debugMode
    $ liftIO do
      hPutStrLn stderr "=== LINKED ==="
      hPrint stderr $ pretty core

  when flags.debugMode do
    inf <- loadInterface moduleName
    hPutStrLn stderr "=== INTERFACE ==="
    hPutTextLn stderr $ render $ pretty inf

  coreOpt <-
    if flags.noOptimize
      then pure core
      else runReader moduleName $ optimizeProgram core >>= Flat.normalize
  when (flags.debugMode && not flags.noOptimize) do
    hPutStrLn stderr "=== OPTIMIZE ==="
    hPrint stderr $ pretty coreOpt
  when flags.testMode do
    save srcPath ".kor-opt" (ViaShow coreOpt)
  lint True coreOpt

  coreLL <- if flags.lambdaLift then runReader moduleName $ lambdalift coreOpt >>= Flat.normalize else pure coreOpt
  when (flags.debugMode && flags.lambdaLift)
    $ liftIO do
      hPutStrLn stderr "=== LAMBDALIFT ==="
      hPrint stderr $ pretty coreLL
  when flags.testMode do
    save srcPath ".kor-lift" (ViaShow coreLL)
  lint True coreLL

  pure coreLL

-- | Compile the parsed AST.
compileFromAST ::
  ( Reader OptimizeOption :> es,
    Reader Flag :> es,
    IOE :> es,
    State (Map ModuleName Interface) :> es,
    State Uniq :> es,
    Workspace :> es
  ) =>
  ArtifactPath ->
  Syntax.Module (Malgo NewParse) ->
  Eff es ()
compileFromAST srcPath parsedAst = do
  let moduleName = parsedAst.moduleName
  coreLL <- compileToCore srcPath parsedAst
  result <- runError @EvalError $ runReader moduleName do
    eval coreLL defaultStdin defaultStdout defaultStderr
  case result of
    Left (cs, err) -> do
      let message = prettyCallStack cs <> "\n" <> show err
      liftIO $ hPutStrLn stderr message
      liftIO exitFailure
    Right _ -> pure ()

-- | Read the source file and parse it, then compile.
compile ::
  ( Reader OptimizeOption :> es,
    Reader Flag :> es,
    IOE :> es,
    State (Map ModuleName Interface) :> es,
    State Uniq :> es,
    Workspace :> es
  ) =>
  FilePath ->
  Eff es ()
compile srcPath = do
  flags <- ask @Flag
  pwd <- pwdPath
  srcModulePath <- parseArtifactPath pwd srcPath
  src <- load srcModulePath ".mlg"
  parseResult <- parse srcPath (convertString @BS.ByteString src)
  parsedAst <- case parseResult of
    Right (_, x) -> pure x
    Left err -> liftIO do
      hPutStrLn stderr $ errorBundlePretty err
      exitFailure
  when flags.debugMode do
    hPutStrLn stderr "=== PARSE ==="
    hPrint stderr $ pretty parsedAst
  runReader parsedAst.moduleName
    $ compileFromAST srcModulePath parsedAst
