{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import           Data.Outputable
import qualified Data.Text.Lazy.IO                 as T
import qualified Language.Malgo.FrontEnd.Driver    as Driver
import qualified Language.Malgo.FrontEnd.Lexer     as Lexer2
import           Language.Malgo.FrontEnd.Loc
import qualified Language.Malgo.FrontEnd.Parser    as Parser2
import qualified Data.Map as Map
import qualified Language.Malgo.FrontEnd.Rename    as Rename
import qualified Language.Malgo.FrontEnd.RnTcEnv   as RnTcEnv
import qualified Language.Malgo.FrontEnd.TypeCheck as TypeCheck
import           Language.Malgo.IR.AST
import qualified Language.Malgo.MiddleEnd.Flatten  as Flatten
import qualified Language.Malgo.MiddleEnd.KNormal  as KNormal
import qualified Language.Malgo.Monad              as Monad
import           Language.Malgo.Old.Driver
import qualified Language.Malgo.Old.Lexer          as Lexer
import           Language.Malgo.Old.Monad
import qualified Language.Malgo.Old.Parser         as Parser
import           Language.Malgo.Pretty
import           LLVM.Pretty
import           Universum

main :: IO ()
main = do
  opt <- parseOpt

  -- c <- isVersion2 opt

  if isVersion2 opt
    then version2 opt
    else do let file = srcName opt
            tokens <- Lexer.lexing () (toString file) =<< readFile file
            let parser = Parser.parseExpr
            let ast = case parser <$> tokens of
                  Left x  -> error $ show x
                  Right x -> x

            u <- newIORef 0
            ll <- compile file ast (UniqSupply u) opt

            T.writeFile (dstName opt) (ppllvm ll)
  where
    version2 :: Opt -> IO ()
    version2 opt = do
      u <- Monad.newUniqSupply
      Monad.runMalgo u Monad.Opt $ do
        let file = srcName opt
        src <- readFile file
        tokens <- Lexer2.lex file src
        let ast = case Parser2.parse <$> tokens of
                    Left x  -> error $ show x
                    Right x -> x

        env <- RnTcEnv.makeRnTcEnv
        putStrLn "-- parsed ast --"
        dump ast
        (renamedAst, env0) <- usingStateT env (Rename.rename ast)
        putStrLn "-- renamed ast --"
        dump renamedAst
        env1 <- executingStateT env0 (TypeCheck.typeCheck renamedAst)
        putStrLn "-- type environment --"
        print $ ppr env1
        knormalizedAst <- KNormal.knormal renamedAst
        putStrLn "-- knormalized ast --"
        dump knormalizedAst
        env2 <- executingStateT env1 (TypeCheck.typeCheck (makeProgram knormalizedAst))
        putStrLn "-- type environment --"
        print $ pPrint $ Map.toList $ view RnTcEnv.variableMap env2
        putStrLn "-- flatten ast --"
        dump $ map (over _3 Flatten.flatten) knormalizedAst
    dump x = putStrLn $ renderStyle (style { lineLength = 80 }) $ pPrint x
    makeProgram ds = Program $ map (\(f, xs, e) -> ScDef noSrcSpan f xs e) ds
