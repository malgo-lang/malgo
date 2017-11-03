module Main where

import qualified Language.Malgo.Beta        as Beta
import qualified Language.Malgo.KNormal     as KNormal
import qualified Language.Malgo.Parser      as Parser
import           Language.Malgo.PrettyPrint
import qualified Language.Malgo.Typing      as Typing
import           System.Environment         (getArgs)
import qualified Text.Parsec.String         as P
import qualified Text.PrettyPrint           as PP
joinE :: (Show e, Show e') => Either e (Either e' a) -> Either String a
joinE (Left x)          = Left (show x)
joinE (Right (Left x))  = Left (show x)
joinE (Right (Right x)) = Right x

main :: IO ()
main = do
  args <- getArgs
  let file = head args
  ast <- P.parseFromFile Parser.parseToplevel file
  print $ PP.vcat . map pretty <$> ast

  let typedAST = joinE $ Typing.typing (mapM (mapM Typing.typeofDecl) ast)
  print $ PP.vcat . map pretty <$> typedAST

  let kNormal = joinE $ KNormal.knormal (mapM (mapM KNormal.transDecl) typedAST)
  print $ PP.vcat . map pretty <$> kNormal

  let beta = joinE $ Beta.betaTrans (mapM (mapM Beta.transDecl) kNormal)
  print $ PP.vcat . map pretty <$> beta
