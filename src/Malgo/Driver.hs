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
import Path (replaceExtension, toFilePath)
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
  act
  where
    moduleName = parsedAst.moduleName
    act = do
      registerModule moduleName srcPath
      dstPath <- replaceExtension ".ll" srcPath.targetPath
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
        liftIO $ T.writeFile (toFilePath dstPath -<.> "kor") $ render $ pretty core

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
        liftIO $ T.writeFile (toFilePath dstPath -<.> "kor.opt") $ render $ pretty coreOpt
      lint True coreOpt

      coreLL <- if flags.lambdaLift then runReader moduleName $ lambdalift coreOpt >>= Flat.normalize else pure coreOpt
      when (flags.debugMode && flags.lambdaLift)
        $ liftIO do
          hPutStrLn stderr "=== LAMBDALIFT ==="
          hPrint stderr $ pretty coreLL
      when flags.testMode do
        liftIO $ T.writeFile (toFilePath dstPath -<.> "kor.opt.lift") $ render $ pretty coreLL
      lint True coreLL

      -- Optimization after lambda lifting causes code explosion.
      -- The effect of lambda lifting is expected to be fully realized by backend's optimization.
      -- TODO: Can we only optimize the code that has been lambda lifted?
      -- TODO: Improve the function inliner to reduce the code explosion.
      -- TODO: Add more information to `call` instruction to improve the function inliner.
      -- Or simply skip inlining and do other optimizations.

      -- On M2 MBA, optimization after lambda lifting causes segmentation fault.
      -- This is probably caused by lack of memory due to handling extremely large ASTs. I don't know the details.
      -- coreLLOpt <- if not env.noOptimize && env.lambdaLift then optimizeProgram OptimizeEnv {uniqSupply, moduleName, debugMode = env.debugMode, option = env.optimizeOption} coreLL else pure coreLL
      -- when (env.debugMode && env.lambdaLift && not env.noOptimize) $
      --   liftIO do
      --     hPutStrLn stderr "=== OPTIMIZE AFTER LAMBDALIFT ==="
      --     hPrint stderr $ pretty coreLLOpt
      -- when env.testMode do
      --   liftIO $ writeFile (env.dstPath -<.> "kor.opt.lift.opt") $ render $ pretty coreLLOpt
      -- lint True coreLLOpt
      Uniq i <- get @Uniq
      let srcRelPath = toFilePath srcPath.relPath
      LLVM.codeGen srcRelPath (toFilePath dstPath) moduleName (searchMain $ Map.toList dsEnv._nameEnv) i coreLL
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
