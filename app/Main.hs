{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import           Data.Outputable
import qualified Data.Text.Lazy.IO                as T
import qualified Language.Malgo.FrontEnd.Driver   as Driver
import qualified Language.Malgo.FrontEnd.Lexer    as Lexer2
import qualified Language.Malgo.MiddleEnd.Flatten as Flatten
import qualified Language.Malgo.FrontEnd.Parser   as Parser2
import qualified Language.Malgo.MiddleEnd.KNormal as KNormal
import qualified Language.Malgo.Monad             as Monad
import           Language.Malgo.Old.Driver
import qualified Language.Malgo.Old.Lexer         as Lexer
import           Language.Malgo.Old.Monad
import qualified Language.Malgo.Old.Parser        as Parser
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
        putStrLn "-- parsed ast --"
        print $ pPrint ast
        (ast', env) <- Driver.frontend ast
        putStrLn "-- type checked ast --"
        print $ pPrint ast'
        putStrLn "-- type environment --"
        print $ ppr env
        (ast'', env') <- usingStateT env (KNormal.knormal ast')
        putStrLn "-- knormalized ast --"
        print $ pPrint ast''
        putStrLn "-- type environment --"
        print $ ppr env'
        putStrLn "-- flatten ast --"
        print $ pPrint $ map (over _3 Flatten.flatten) ast''
