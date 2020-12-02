{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Griff.Driver (compile) where

import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import Koriel.Core.CodeGen (codeGen)
import Koriel.Core.Core
import Koriel.Core.Flat (flat)
import Koriel.Core.LambdaLift (lambdalift)
import Koriel.Core.Lint (lintProgram, runLint)
import Koriel.Core.Optimize (optimizeProgram)
import Koriel.MonadUniq
import Koriel.Pretty
import qualified LLVM.AST as L
import LLVM.Context (withContext)
import LLVM.Module (moduleLLVMAssembly, withModuleFromAST)
import LLVM.Pretty (ppllvm)
import Language.Griff.Desugar (desugar)
import Language.Griff.Infer (typeCheck)
import Language.Griff.Interface (buildInterface, loadInterface, prettyInterface, storeInterface)
import Language.Griff.Parser (parseGriff)
import Language.Griff.Prelude
import Language.Griff.Rename (rename)
import Language.Griff.RnEnv (genRnEnv)
import qualified Language.Griff.Syntax as Syntax
import qualified Language.Griff.TcEnv as T
import System.IO
  ( hPrint,
    hPutStrLn,
    stderr,
  )
import Text.Megaparsec
  ( errorBundlePretty,
  )
import qualified Text.PrettyPrint.HughesPJ as P

compile :: Opt -> IO ()
compile opt = do
  src <- T.readFile (srcName opt)
  moduleAst <- case parseGriff (srcName opt) src of
    Right x -> pure x
    Left err -> error $ errorBundlePretty err
  when (dumpParsed opt) $ do
    hPutStrLn stderr "=== PARSE ==="
    hPrint stderr $ pPrint moduleAst
  void $
    runReaderT ?? opt $
      unGriffM $
        runUniqT ?? UniqSupply 0 $ do
          rnEnv <- genRnEnv
          ds' <- rename rnEnv moduleAst
          when (dumpRenamed opt) $
            liftIO $ do
              hPutStrLn stderr "=== RENAME ==="
              hPrint stderr $ P.sep $ P.punctuate ";" $ map pPrint ds'
          (tcEnv, bg) <- typeCheck rnEnv ds'
          when (dumpTyped opt) $
            liftIO $ do
              hPutStrLn stderr "=== TYPE CHECK ==="
              hPrint stderr $ pPrint $ Map.toList $ view T.varEnv tcEnv
              hPrint stderr $ Map.toList $ view T.typeEnv tcEnv
              hPrint stderr $ pPrint bg
          (dsEnv, core) <- desugar tcEnv bg
          when (dumpDesugar opt) $
            liftIO $ do
              hPutStrLn stderr "=== DESUGAR ==="
              hPrint stderr $ pPrint $ over appProgram flat core
          let inf = buildInterface dsEnv
          storeInterface inf
          when (debugMode opt) $ do
            Just inf <- loadInterface (Syntax._moduleName moduleAst)
            liftIO $ do
              hPutStrLn stderr "=== INTERFACE ==="
              hPutStrLn stderr $ renderStyle (style {lineLength = 120}) $ prettyInterface inf
          runLint $ lintProgram core
          coreOpt <- if noOptimize opt then pure core else optimizeProgram (inlineSize opt) core
          when (dumpDesugar opt && not (noOptimize opt)) $
            liftIO $ do
              hPutStrLn stderr "=== OPTIMIZE ==="
              hPrint stderr $ pPrint $ over appProgram flat coreOpt
          runLint $ lintProgram coreOpt
          coreLL <- if noLambdaLift opt then pure coreOpt else lambdalift coreOpt
          when (dumpDesugar opt && not (noLambdaLift opt)) $
            liftIO $ do
              hPutStrLn stderr "=== LAMBDALIFT ==="
              hPrint stderr $ pPrint $ over appProgram flat coreLL
          coreLLOpt <- if noOptimize opt then pure coreLL else optimizeProgram (inlineSize opt) coreLL
          when (dumpDesugar opt && not (noLambdaLift opt) && not (noOptimize opt)) $
            liftIO $ do
              hPutStrLn stderr "=== LAMBDALIFT OPTIMIZE ==="
              hPrint stderr $ pPrint $ over appProgram flat coreLLOpt
          llvmir <- codeGen coreLLOpt

          let llvmModule =
                L.defaultModule
                  { L.moduleName = fromString $ srcName opt,
                    L.moduleSourceFileName = fromString $ srcName opt,
                    L.moduleDefinitions = llvmir
                  }
          if viaBinding opt
            then liftIO $
              withContext $ \ctx ->
                BS.writeFile (dstName opt) =<< withModuleFromAST ctx llvmModule moduleLLVMAssembly
            else
              liftIO $
                TL.writeFile (dstName opt) $
                  ppllvm llvmModule
