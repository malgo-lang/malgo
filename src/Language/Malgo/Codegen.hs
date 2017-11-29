{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecursiveDo                #-}
module Language.Malgo.Codegen where

import           Language.Malgo.IRBuilder
import           Language.Malgo.KNormal   (Type (..))
import           Language.Malgo.MIR       (Block (..), Instr, Val (..))
import           Language.Malgo.Utils

import           Control.Monad.State
import           Data.ByteString          (ByteString)
import           Data.Char
import           Data.Maybe
import           Data.String

import qualified LLVM.AST                 as AST
import qualified LLVM.AST.AddrSpace       as A
import qualified LLVM.AST.Constant        as C
import qualified LLVM.AST.Global          as G
import qualified LLVM.AST.Name            as N
import qualified LLVM.AST.Operand         as O
import qualified LLVM.AST.Type            as T
import qualified LLVM.Context             as Context
import qualified LLVM.Module              as M

emit :: AST.Module -> IO ByteString
emit mod =
  Context.withContext $
  \ctx ->
     M.withModuleFromAST ctx mod M.moduleLLVMAssembly

trans :: String -> [Instr] -> Block -> Either String AST.Module
trans name toplevel main = let mmodules = execModuleBuilder emptyModuleBuilder $ do
                                 initModuleBuilder
                                 gtab <- transToplevel [] toplevel
                                 transMain gtab main
                           in case mmodules of
                                Left x  -> Left x
                                Right x -> Right (mkModule name x)
  where mkModule name ds = AST.defaultModule { AST.moduleName = (fromString name)
                                             , AST.moduleSourceFileName = (fromString name)
                                             , AST.moduleDefinitions = ds
                                             }

type SymbolTable = [(Id, O.Operand)]

initModuleBuilder :: ModuleBuilder ()
initModuleBuilder = do
  put emptyModuleBuilder
  deftype "unit" (T.StructureType False [])

transToplevel :: SymbolTable -> [Instr] -> ModuleBuilder SymbolTable
transToplevel tab (((name, _), String x) : xs) = do
  let ty = T.ArrayType
           (fromInteger (toInteger (length x + 1)))
           (T.IntegerType 8)
  ref <- defvar (fromId name) ty (C.Array (T.IntegerType 8)
                                   (map (C.Int 8 . toInteger . ord) x
                                     ++ [C.Int 8 0]))
  transToplevel ((name, ref):tab) xs
transToplevel tab (((name, _), Fun fv params retty body) : xs) = do
  tab' <- transFun tab name fv params retty body
  transToplevel tab' xs
transToplevel tab [] = return tab
transToplevel tab xs = ModuleBuilder $ lift . Left $ "\nunreacheable: transToplevel (" ++ show tab ++ ") (" ++ show xs ++ ")"

transType :: Type -> T.Type
transType (NameTy name) =
  fromMaybe (error $ show name ++ " (type) is not found.") (lookup name typeMap)
  where
    typeMap = [ (Raw "Int", T.IntegerType 32)
              , (Raw "Float", T.FloatingPointType T.DoubleFP)
              , (Raw "Bool", T.IntegerType 1)
              , (Raw "Char", T.IntegerType 8)
              , (Raw "String", T.PointerType (T.IntegerType 8)
                                      (A.AddrSpace 0))
              , (Raw "Unit", T.NamedTypeReference "unit")
              ]
transType (FunTy (TupleTy xs) retty) =
  let xs' = map transType xs
      retty' = transType retty
  in T.FunctionType retty' xs' False
transType x = error $ "unreachable: transType (" ++ show x ++ ")"

transMain
  :: SymbolTable -- Global definitions
  -> Block
  -> ModuleBuilder ()
transMain tab body = do
  let label = "__malgo_main"
      retty = NameTy (Raw "Unit")
  _ <- transFun tab label [] [] retty body
  return ()

transFun
  :: SymbolTable
  -> Id
  -> [(Id, Type)] -- 自由変数
  -> [(Id, Type)] -- 仮引数
  -> Type -- Return type
  -> Block
  -> ModuleBuilder SymbolTable
transFun tab name [] params retty body = do
  let label = fromId name
      retty' = transType retty
      params' = [(ty, Just nm) |
                 (ty, nm) <- zip
                               (map (transType . snd) params)
                               (map (fromId . fst) params)]
  let funty = ptr (T.FunctionType retty' (map fst params') False)
      funref = cons $ C.GlobalReference funty label
  fn <- (defun
          label
          params'
          retty'
          (transBody tab (name, funref) body (map fst params)))
  return ((name, fn) : tab)
transFun tab name fv params retty body = do
  ModuleBuilder $ lift . Left $ "closure is not supported now."

transBody
  :: SymbolTable
  -> (Id, O.Operand) -- 関数へのreference
  -> Block
  -> [Id] -- 仮引数名のリスト
  -> [O.Operand] -- 仮引数のlocal referenceのリスト
  -> InstrBuilder ()
transBody tab fn (Block name instrs) paramIds paramRefs = do
  _ <- block `named` fromId name
  transBody' (zip paramIds paramRefs ++ (fn : tab)) instrs

transBody'
  :: SymbolTable
  -> [Instr]
  -> InstrBuilder ()
transBody' tab instrs =
  case instrs of
    (((name, ty), val) : xs) -> do
      reference <- transInstr (transType ty) val
      transBody' ((name, reference) : tab) xs
    [] -> return ()
  where ref x = lookup x tab
        transInstr ty (Int x) = do
          a <- alloca ty Nothing
          store a $ cons (int x)
          load ty a
        transInstr ty (Float x) = do
          a <- alloca ty Nothing
          store a $ cons (double x)
          load ty a
        transInstr ty (Bool x) = do
          a <- alloca ty Nothing
          store a $ cons (bool x)
          load ty a
        transInstr ty (Char x) = do
          a <- alloca ty Nothing
          store a $ cons (char x)
          load ty a
        transInstr _ (String x) = InstrBuilder $ lift . Left $ "unreachable: transInstr _ _ (String " ++ show x ++ ")"
        transInstr ty Unit = do
          a <- alloca ty Nothing
          store a $ cons (undef ty)
          load ty a
        transInstr ty (App fn args) =
          case ref (fst fn) of
            Nothing -> InstrBuilder $ lift . Left $ show fn ++ " is not found."
            Just f -> do
              let args' = map (ref . fst) args
              if Nothing `elem` args'
                then InstrBuilder $ lift . Left $ show args ++ " are not found."
                else call ty f (map (\(Just x) -> x) args')

transIfBlock
  :: SymbolTable
  -> Block
  -> InstrBuilder (O.Operand, N.Name) -- phi関数の引数になる
transIfBlock = undefined
