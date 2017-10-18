module Main where

import           Control.Arrow                     ((&&&))
import           Control.Monad.State
import qualified Language.Malgo.Alpha              as Alpha
import qualified Language.Malgo.Assoc              as Assoc
import qualified Language.Malgo.Beta               as Beta
import qualified Language.Malgo.HIR                as HIR
import qualified Language.Malgo.InsertRET          as InsertRET
import qualified Language.Malgo.KNormal            as KNormal
import qualified Language.Malgo.MIR                as MIR
import qualified Language.Malgo.Parser             as Parser
import qualified Language.Malgo.PrettyPrint.HIR    as PH
import qualified Language.Malgo.PrettyPrint.MIR    as PM
import qualified Language.Malgo.PrettyPrint.Syntax as PS
import qualified Language.Malgo.Syntax             as Syntax
import qualified Language.Malgo.Typing             as Typing
import           System.Environment                (getArgs)
import qualified Text.Parsec.String                as P
import qualified Text.PrettyPrint                  as Pretty

main :: IO ()
main = do
  args <- getArgs
  let file = head args
  result <- P.parseFromFile Parser.parseToplevel file
  case result of
    Left err  -> print err
    Right ast -> do
      putStrLn "Format:"
      print $ Pretty.sep (map PS.prettyDecl ast)
      putStrLn "AST:"
      print ast

      putStrLn "\nTyped AST(HIR 'Typed):"
      let typedAst = Typing.typing ast
      case typedAst of
        Right xs -> mapM_ (print . PH.pretty . Assoc.flatten) xs
        Left x   -> putStrLn $ "typing error: " ++ x

      putStrLn "\nKNormalized AST(HIR 'KNormal):"
      let kAst = case typedAst of
                   Right tAst -> Right $ KNormal.kNormalize tAst
                   Left x     -> Left x
      case kAst of
        Right xs -> mapM_ (print . PH.pretty . Assoc.flatten) xs
        Left _   -> return ()

      putStrLn "\nAlpha-converted AST(HIR 'KNormal):"
      let alpha = case kAst of
            Right xs -> Right $ map (Alpha.trans) xs
            Left x   -> Left x

      case alpha of
        Right xs -> mapM_ (print . PH.pretty . Assoc.flatten . fst) xs
        Left _   -> return ()

      putStrLn "\nBeta-reduced AST(HIR 'KNormal):"
      let beta = case alpha of
            Right xs -> Right $ map (Beta.trans . fst) xs
            Left x   -> Left x

      case beta of
        Right xs -> mapM_ (print . PH.pretty . Assoc.flatten . fst) xs
        Left _   -> return ()

      putStrLn "\nMIR:"
      let mir = case beta of
            Right xs -> Right $ map (MIR.trans . Assoc.flatten . fst) xs
            Left x   -> Left x

      case mir of
        Right xs -> mapM_ (print . PM.pretty . fst) xs
        Left _   -> return ()

      putStrLn "\nMIR(ret inserted):"
      let mir' = case mir of
            Right xs -> Right $ map (InsertRET.trans . fst) xs
            Left x   -> Left x

      case mir' of
        Right xs -> mapM_ (print . PM.pretty . fst) xs
        Left _   -> return ()
