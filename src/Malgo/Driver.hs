-- | Malgo.Driver is the entry point of `malgo to-ll`.
module Malgo.Driver (compile, compileFromAST, withDump) where

import Control.Exception (IOException, catch)
import Data.ByteString qualified as BS
import Data.Set qualified as Set
import Data.Traversable (for)
import Effectful
import Effectful.Error.Static (Error)
import Effectful.Reader.Static
import Effectful.State.Static.Local
import Malgo.Infer
import Malgo.Interface (Interface, buildInterface)
import Malgo.Module
import Malgo.Parser.Pass (ParserPass (..))
import Malgo.Pass (CompileError, Pass (..), runCompileError)
import Malgo.Prelude
import Malgo.Refine
import Malgo.Rename
import Malgo.Sequent.Core (Join)
import Malgo.Sequent.Core qualified as Sequent
import Malgo.Sequent.Core.Flat (FlatPass (..))
import Malgo.Sequent.Core.Join (JoinPass (..))
import Malgo.Sequent.Eval (EvalPass (..), Handlers (..))
import Malgo.Sequent.ToCore (ToCorePass (..))
import Malgo.Sequent.ToFun (ToFunPass (..))
import Malgo.Syntax qualified as Syntax
import Malgo.Syntax.Extension
import System.IO (hPutChar)
import System.IO qualified as IO

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

-- | Compile the parsed AST to Core representation.
compileToCore ::
  ( Reader Flag :> es,
    Error CompileError :> es,
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
  rnEnv <- genBuiltinRnEnv
  (renamedAst, rnState) <- withDump flags.debugMode "=== RENAME ===" do
    runPass RenamePass (parsedAst, rnEnv)
  (typedAst, tcEnv, kindCtx) <- withDump flags.debugMode "=== TYPE CHECK ===" do
    runPass InferPass (renamedAst, rnEnv)
  refinedAst <- withDump flags.debugMode "=== REFINE ===" do
    runPass RefinePass (typedAst, tcEnv)

  let inf = buildInterface moduleName rnState tcEnv kindCtx
  save srcPath ".mlgi" (ViaStore inf)

  generateSequent srcPath rnState refinedAst

generateSequent ::
  ( IOE :> es,
    State Uniq :> es,
    Workspace :> es,
    Error CompileError :> es
  ) =>
  ArtifactPath ->
  RnState ->
  Syntax.Module (Malgo Refine) ->
  Eff es (Sequent.Program Join)
generateSequent srcPath rnState Syntax.Module {..} = do
  program <- runReader moduleName do
    runPass ToFunPass moduleDefinition
      >>= runPass ToCorePass
      >>= runPass FlatPass
      >>= runPass JoinPass
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
    Error CompileError :> es,
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
  runPass EvalPass (moduleName, Handlers {..}, core)

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
  runCompileError do
    (_, parsedAst) <- runPass ParserPass (srcPath, convertString @BS.ByteString src)
    when flags.debugMode do
      hPutStrLn stderr "=== PARSE ==="
      hPrint stderr $ pretty parsedAst
    runReader parsedAst.moduleName
      $ compileFromAST srcModulePath parsedAst
