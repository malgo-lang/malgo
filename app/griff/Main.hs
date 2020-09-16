{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.Exception (catch, displayException)
import qualified Data.Map as Map
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import qualified LLVM.AST as L
import LLVM.Pretty (ppllvm)
import Language.Griff.Desugar (desugar)
import Language.Griff.Grouping
import Language.Griff.Parser (pTopLevel)
import Language.Griff.Rename (rename)
import Language.Griff.RnEnv
import qualified Language.Griff.TcEnv as T
import Language.Griff.Typing (typeCheck)
import Language.Malgo.Core.CodeGen
import Language.Malgo.Core.Flat (flat)
import Language.Malgo.Core.Optimize (optimize)
import Language.Malgo.IR.Core
import Language.Malgo.Monad
import Language.Malgo.Prelude
import Language.Malgo.Pretty
import System.Exit (exitFailure)
import System.IO (hPrint, hPutStr, hPutStrLn, stderr)
import Text.Megaparsec (errorBundlePretty, parse)
import qualified Text.PrettyPrint as P
import Language.Malgo.Core.Lint (lint)

main :: IO ()
main =
  compile `catch` \e -> do
    hPutStr stderr (displayException (e :: Bug))
    exitFailure

compile :: IO ()
compile = do
  src <- T.getContents
  ds <- case parse pTopLevel "<stdin>" src of
    Right ds -> pure ds
    Left err -> error $ errorBundlePretty err
  hPutStrLn stderr "=== PARSE ==="
  hPrint stderr $ P.sep $ P.punctuate ";" $ map pPrint ds
  void $
    runUniqT ?? UniqSupply 0 $ do
      rnState <- genRnState
      rnEnv <- genRnEnv

      liftIO $ hPutStrLn stderr "=== RENAME ==="
      ds' <- rename rnState rnEnv ds
      liftIO $ hPrint stderr $ P.sep $ P.punctuate ";" $ map pPrint ds'

      liftIO $ hPutStrLn stderr "=== CALL GRAPH ==="
      let bindGroup = makeBindGroup ds'
      liftIO $ hPrint stderr $ pPrint $ bindGroup ^. scDefs

      liftIO $ hPutStrLn stderr "=== TYPE CHECK ==="
      (bg, tcEnv) <- typeCheck rnEnv ds'
      liftIO $ hPrint stderr $ pPrint $ Map.toList $ view T.varEnv tcEnv
      liftIO $ hPrint stderr $ Map.toList $ view T.typeEnv tcEnv
      liftIO $ hPrint stderr $ pPrint $ Map.toList $ view T.tyConEnv tcEnv
      liftIO $ hPrint stderr $ pPrint bg

      liftIO $ hPutStrLn stderr "=== DESUGAR ==="
      core <- desugar tcEnv bg
      liftIO $ hPrint stderr $ pPrint $ flat core
      lint core
      liftIO $ hPutStrLn stderr "=== OPTIMIZE ==="
      let coreOpt = optimize 10 core
      liftIO $ hPrint stderr $ pPrint $ flat coreOpt
      -- TODO: 最適化するか否かをフラグで指定できるように変更
      -- optimizeにバグがある
      llvmir <- codeGen (Program {topBinds = [], topFuncs = [], mainExp = flat core})
      let mod =
            L.defaultModule
              { L.moduleName = "<stdin>",
                L.moduleSourceFileName = "<stdin>",
                L.moduleDefinitions = llvmir
              }
      liftIO $ TL.putStrLn (ppllvm mod)