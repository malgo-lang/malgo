-- | Malgo.Driver is the entry point of `malgo to-ll`.
module Malgo.Driver (compile, compileFromAST, withDump, failIfError) where

import Control.Exception (IOException, catch)
import Data.ByteString qualified as BS
import Data.Set qualified as Set
import Data.Traversable (for)
import Effectful
import Effectful.Error.Static (CallStack, prettyCallStack, runError)
import Effectful.Reader.Static
import Effectful.State.Static.Local
import Malgo.Core.Flat qualified as Flat
import Malgo.Core.LambdaLift (lambdalift)
import Malgo.Core.Lint (lint)
import Malgo.Core.Optimize (OptimizeOption, optimizeProgram)
import Malgo.Core.Syntax (Program)
import Malgo.Core.Type (Type)
import Malgo.Desugar.DsState (DsState (..))
import Malgo.Desugar.Pass (desugar)
import Malgo.Id (Meta (..))
import Malgo.Infer.Pass qualified as Infer
import Malgo.Infer.TcEnv (TcEnv (..))
import Malgo.Interface (Interface, buildInterface, loadInterface)
import Malgo.Link qualified as Link
import Malgo.Module
import Malgo.Monad
import Malgo.MonadUniq
import Malgo.Parser (parse)
import Malgo.Prelude
import Malgo.Refine.Pass (refine)
import Malgo.Rename.Pass (rename)
import Malgo.Rename.RnEnv qualified as RnEnv
import Malgo.Rename.RnState (RnState (..))
import Malgo.Sequent.Core (Join)
import Malgo.Sequent.Core qualified as Sequent
import Malgo.Sequent.Core.Flat (flatProgram)
import Malgo.Sequent.Core.Join (joinProgram)
import Malgo.Sequent.Eval (EvalError, Handlers (..), evalProgram)
import Malgo.Sequent.ToCore (toCore)
import Malgo.Sequent.ToFun (toFun)
import Malgo.Syntax qualified as Syntax
import Malgo.Syntax.Extension
import System.Exit (exitFailure)
import System.IO (hPutChar)
import System.IO qualified as IO
import System.IO.Streams qualified as Streams
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
  Eff es (Sequent.Program Join)
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

  _ <- generateCore srcPath moduleName flags rnState tcEnv refinedAst
  generateSequent srcPath moduleName rnState refinedAst

generateSequent ::
  ( IOE :> es,
    State Uniq :> es,
    Workspace :> es
  ) =>
  ArtifactPath ->
  ModuleName ->
  RnState ->
  Syntax.Module (Malgo Refine) ->
  Eff es (Sequent.Program Join)
generateSequent srcPath moduleName rnState refinedAst = do
  program <- runReader moduleName $ toFun refinedAst.moduleDefinition >>= toCore >>= flatProgram >>= joinProgram
  save srcPath ".sqt" (ViaStore program)

  linkSequent rnState.dependencies program

linkSequent :: (Workspace :> es, IOE :> es) => Set ModuleName -> Sequent.Program Join -> Eff es (Sequent.Program Join)
linkSequent dependencies program = do
  deps <- for (Set.toList dependencies) \dep -> do
    path <- getModulePath dep
    ViaStore x <- load path ".sqt"
    pure x
  let program' =
        Sequent.Program
          { definitions =
              program.definitions
                <> concatMap (\Sequent.Program {definitions} -> definitions) deps,
            dependencies = []
          }
  pure program'

generateCore ::
  ( Reader OptimizeOption :> es,
    IOE :> es,
    State (Map ModuleName Interface) :> es,
    State Uniq :> es,
    Workspace :> es
  ) =>
  ArtifactPath ->
  ModuleName ->
  Flag ->
  RnState ->
  TcEnv ->
  Syntax.Module (Malgo Refine) ->
  Eff es (Program (Meta Type))
generateCore srcPath moduleName flags rnState tcEnv refinedAst = do
  (dsState, core) <- desugar tcEnv refinedAst

  core <- do
    core <- runReader moduleName $ Flat.normalize core
    _ <- withDump flags.debugMode "=== DESUGAR ===" $ pure core
    save srcPath ".mo" (ViaStore core)

    let inf = buildInterface moduleName rnState tcEnv dsState
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
  core <- compileToCore srcPath parsedAst
  stdin <- liftIO $ Streams.makeInputStream $ fmap Just getChar `catch` \(_ :: IOException) -> pure Nothing
  stdout <- liftIO $ Streams.makeOutputStream \case
    Just c -> putChar c
    Nothing -> pure ()
  stderr <- liftIO $ Streams.makeOutputStream \case
    Just c -> hPutChar IO.stderr c
    Nothing -> pure ()
  result <-
    runError @EvalError
      $ runReader moduleName
      $ runReader Handlers {stdin, stdout, stderr}
      $ evalProgram core
  case result of
    Left (cs, err) -> do
      let message = prettyCallStack cs <> "\n" <> show err
      liftIO $ hPutStrLn IO.stderr message
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
