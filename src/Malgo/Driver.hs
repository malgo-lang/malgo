-- | Malgo.Driver is the entry point of `malgo to-ll`.
module Malgo.Driver (compile, compileFromAST, withDump) where

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.String.Conversions.Monomorphic (toString)
import Data.Text.IO qualified as T
import Effectful
import Effectful.Reader.Static
import Effectful.State.Static.Local
import Error.Diagnose (TabSize (..), WithUnicode (..), addFile, prettyDiagnostic)
import Error.Diagnose.Compat.Megaparsec
import Malgo.Core.CodeGen.LLVM qualified as LLVM
import Malgo.Core.Flat qualified as Flat
import Malgo.Core.LambdaLift (lambdalift)
import Malgo.Core.Lint (lint)
import Malgo.Core.Optimize (OptimizeOption, optimizeProgram)
import Malgo.Core.Syntax (Program)
import Malgo.Core.Type (Type)
import Malgo.Desugar.DsState (_nameEnv)
import Malgo.Desugar.Pass (desugar)
import Malgo.Id (Id (Id, moduleName, name, sort), IdSort (External), Meta (..))
import Malgo.Infer.Pass qualified as Infer
import Malgo.Interface (Interface, buildInterface, loadInterface)
import Malgo.Link qualified as Link
import Malgo.Module
import Malgo.Monad
import Malgo.MonadUniq
import Malgo.Parser (parseMalgo)
import Malgo.Prelude
import Malgo.Refine.Pass (refine)
import Malgo.Rename.Pass (rename)
import Malgo.Rename.RnEnv qualified as RnEnv
import Malgo.Syntax qualified as Syntax
import Malgo.Syntax.Extension
import Path (toFilePath)
import Prettyprinter qualified as PrettyPrinter
import Prettyprinter.Render.Text qualified as PrettyPrinter
import System.Exit (exitFailure)
import System.FilePath ((-<.>))
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
    hPrint stderr $ pretty result
  pure result

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
  Syntax.Module (Malgo 'Parse) ->
  Eff es (Program (Meta Type), Map Id (Meta Type))
compileToCore srcPath parsedAst = do
  let moduleName = parsedAst.moduleName
  registerModule moduleName srcPath
  flags <- ask @Flag

  when flags.debugMode $ liftIO do
    hPutStrLn stderr "=== PARSED ==="
    hPrint stderr $ pretty parsedAst
  rnEnv <- RnEnv.genBuiltinRnEnv
  (renamedAst, rnState) <- withDump flags.debugMode "=== RENAME ===" $ rename rnEnv parsedAst
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
    liftIO $ T.writeFile (toFilePath srcPath.targetPath -<.> "kor") $ render $ pretty core

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
    liftIO $ T.writeFile (toFilePath srcPath.targetPath -<.> "kor.opt") $ render $ pretty coreOpt
  lint True coreOpt

  coreLL <- if flags.lambdaLift then runReader moduleName $ lambdalift coreOpt >>= Flat.normalize else pure coreOpt
  when (flags.debugMode && flags.lambdaLift)
    $ liftIO do
      hPutStrLn stderr "=== LAMBDALIFT ==="
      hPrint stderr $ pretty coreLL
  when flags.testMode do
    liftIO $ T.writeFile (toFilePath srcPath.targetPath -<.> "kor.opt.lift") $ render $ pretty coreLL
  lint True coreLL

  pure (coreLL, dsEnv._nameEnv)

-- | Compile the Core representation to LLVM module.
compileToLLVM ::
  ( IOE :> es,
    State Uniq :> es
  ) =>
  ArtifactPath ->
  ModuleName ->
  Program (Meta Type) ->
  Map Id (Meta Type) ->
  Eff es ()
compileToLLVM srcPath moduleName coreLL nameEnv = do
  Uniq i <- get @Uniq
  let srcRelPath = toFilePath srcPath.relPath
  let dstPath = toFilePath srcPath.targetPath -<.> "ll"
  LLVM.codeGen srcRelPath dstPath moduleName (searchMain $ Map.toList nameEnv) i coreLL
  where
    -- エントリーポイントとなるmain関数を検索する
    searchMain :: [(Id, Meta b)] -> Maybe (Meta b)
    searchMain ((griffId@Id {sort = External}, coreId) : _)
      | griffId.name
          == "main"
          && griffId.moduleName
          == moduleName =
          Just coreId
    searchMain (_ : xs) = searchMain xs
    searchMain _ = Nothing

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
  Syntax.Module (Malgo 'Parse) ->
  Eff es ()
compileFromAST srcPath parsedAst = do
  let moduleName = parsedAst.moduleName
  (coreLL, nameEnv) <- compileToCore srcPath parsedAst
  compileToLLVM srcPath moduleName coreLL nameEnv

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
  parseResult <- parseMalgo srcPath (convertString @BS.ByteString src)
  parsedAst <- case parseResult of
    Right x -> pure x
    Left err -> liftIO do
      let diag = errorDiagnosticFromBundle @Text Nothing "Parse error on input" Nothing err
      let diag' = addFile diag srcPath (toString src)
      let message =
            convertString
              $ PrettyPrinter.renderStrict
              $ PrettyPrinter.layoutPretty PrettyPrinter.defaultLayoutOptions
              $ PrettyPrinter.unAnnotate
              $ prettyDiagnostic WithUnicode (TabSize 4) diag'
      BS.hPutStr stderr message -- ByteString.hPutStr is an atomic operation.
      hFlush stderr
      exitFailure
  when flags.debugMode do
    hPutStrLn stderr "=== PARSE ==="
    hPrint stderr $ pretty parsedAst
  runReader parsedAst.moduleName
    $ compileFromAST srcModulePath parsedAst
