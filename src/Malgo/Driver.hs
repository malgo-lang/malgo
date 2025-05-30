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
import Malgo.Infer.Pass qualified as Infer
import Malgo.Infer.TcEnv (TcEnv (..))
import Malgo.Interface (Interface, buildInterface)
import Malgo.Module
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
  ( Reader Flag :> es,
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
  (typedAst, tcEnv, kindCtx) <- failIfError <$> Infer.infer rnEnv renamedAst
  _ <- withDump flags.debugMode "=== TYPE CHECK ===" $ pure typedAst
  refinedAst <- withDump flags.debugMode "=== REFINE ===" $ refine tcEnv typedAst

  let inf = buildInterface moduleName rnState tcEnv kindCtx
  save srcPath ".mlgi" (ViaStore inf)

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

-- | Compile the parsed AST.
compileFromAST ::
  ( Reader Flag :> es,
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
  let stdin = fmap Just getChar `catch` \(_ :: IOException) -> pure Nothing
  let stdout = putChar
  let stderr = hPutChar IO.stderr
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
  ( Reader Flag :> es,
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
