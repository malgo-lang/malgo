{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Language.Malgo.Codegen where

import           Control.Monad.State
import           Data.Char
import           Data.Function
import           Data.List
import           Data.Monoid
import           Data.String

import           Language.Malgo.KNormal      (Type (..))
-- import           Language.Malgo.LLVM
import           Language.Malgo.MIR          hiding (count)
import           Language.Malgo.Utils

import qualified Data.ByteString             as BS
import qualified Data.ByteString.Short       as BS
import qualified Data.Map                    as Map

import qualified LLVM.AST                    as AST
import qualified LLVM.AST.AddrSpace          as AddrSpace
import qualified LLVM.AST.CallingConvention  as CC
import qualified LLVM.AST.Constant           as C
import qualified LLVM.AST.Float              as F
import qualified LLVM.AST.Global             as Global
import qualified LLVM.AST.Name               as Name
import qualified LLVM.AST.ParameterAttribute as PA
import qualified LLVM.AST.Type               as T hiding (double)
import qualified LLVM.Context                as Context
import qualified LLVM.Module                 as Module

data LLVMState = LLVMState
  {
    llvmModule :: AST.Module
  }
  deriving Show

newtype LLVM a = LLVM (StateT LLVMState (Either String) a)
  deriving (Functor, Applicative, Monad, MonadState LLVMState)

-- runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM :: LLVMState -> LLVM a -> Either String LLVMState
runLLVM mod (LLVM m) = execStateT m mod

initLLVMState :: BS.ShortByteString -> LLVMState
initLLVMState label = LLVMState
  {
    llvmModule = AST.defaultModule { AST.moduleName = label
                                   , AST.moduleDefinitions =
                                     [AST.TypeDefinition "unit" (Just (T.StructureType False []))]
                                   }
  }

-- emit :: LLVMState -> BS.ByteString
emit :: LLVMState -> IO BS.ByteString
emit (LLVMState mod) = Context.withContext $ \ctx ->
  Module.withModuleFromAST ctx mod Module.moduleLLVMAssembly


addDefn :: AST.Definition -> LLVM ()
addDefn d = do
  defs <- gets (AST.moduleDefinitions . llvmModule)
  modify $ \c -> c { llvmModule =
                       (llvmModule c)
                       { AST.moduleDefinitions =
                           defs ++ [d]
                       }
                   }

compFun :: Instr -> LLVM ()
compFun ((name, _), Fun [] params retTy body) = do
  let label = fromId name
  let retTy' = compTy retTy
  params' <- do
    let tys = map (compTy . snd) params
    let nms = map (fromId . fst) params
    return [Global.Parameter ty nm [] | (ty, nm) <- zip tys nms]
  addDefn $ AST.GlobalDefinition AST.functionDefaults
    { Global.name = Name.Name label
    , Global.parameters = (params', False)
    , Global.returnType = retTy'
    , Global.basicBlocks = createBlocks $ compFunBody body
    }

compMain :: Block -> LLVM ()
compMain body = do
  let label = "__malgo_main"
  let retTy = T.NamedTypeReference "unit"
  addDefn $ AST.GlobalDefinition AST.functionDefaults
    { Global.name = Name.Name label
    , Global.parameters = ([], False)
    , Global.returnType = retTy
    , Global.basicBlocks = createBlocks $ compFunBody body
    }

type Names = Map.Map BS.ShortByteString Int

uniqueName :: BS.ShortByteString -> Names -> (BS.ShortByteString, Names)
uniqueName nm ns =
  case Map.lookup nm ns of
    Nothing -> (nm, Map.insert nm 1 ns)
    Just ix -> ( nm <> fromString (show ix)
               , Map.insert nm (ix + 1) ns)

type SymbolTable = [(Name.Name, AST.Operand)]

data CodegenState = CodegenState { currentBlock :: Name.Name
                                 , blocks       :: Map.Map Name.Name BlockState
                                 , blockCount   :: Int
                                 , count        :: Word
                                 , names        :: Names
                                 , symtab       :: SymbolTable
                                 }
  deriving Show

data BlockState = BlockState { idx   :: Int
                             , stack :: [AST.Named AST.Instruction]
                             , term  :: Maybe (AST.Named AST.Terminator)
                             }
  deriving Show

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
 deriving (Functor, Applicative, Monad, MonadState CodegenState)

sortBlocks :: [(Name.Name, BlockState)] -> [(Name.Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

createBlocks :: CodegenState -> [Global.BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ Map.toList (blocks m)

makeBlock :: (Name.Name, BlockState) -> Global.BasicBlock
makeBlock (l, BlockState _ s t) = Global.BasicBlock l (reverse s) (maketerm t)
  where maketerm (Just x) = x
        maketerm Nothing  = error $ "Block has no terminator: " ++ show l

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

emptyCodegen :: CodegenState
emptyCodegen = CodegenState { currentBlock = Name.Name "entry"
                            , blocks = Map.empty
                            , blockCount = 1
                            , count = 0
                            , names = Map.empty
                            , symtab = []
                            }

execCodegen :: Codegen a -> CodegenState
execCodegen m = execState (runCodegen m) emptyCodegen

entry :: Codegen Name.Name
entry = gets currentBlock

addBlock :: BS.ShortByteString -> Codegen Name.Name
addBlock bname = do
  bls <- gets blocks
  ix <- gets blockCount
  nms <- gets names
  let new = emptyBlock ix
      (qname, supply) = uniqueName bname nms
  modify $ \s -> s { blocks = Map.insert (Name.Name qname) new bls
                   , blockCount = ix + 1
                   , names = supply
                   }
  return (Name.Name qname)

setBlock :: Name.Name -> Codegen Name.Name
setBlock name = do
  modify $ \s -> s { currentBlock = name }
  return name

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s { blocks = Map.insert active new (blocks s) }

getBlock :: Codegen Name.Name
getBlock = gets currentBlock

current :: Codegen BlockState
current = do
  c <- gets currentBlock
  blks <- gets blocks
  case Map.lookup c blks of
    Just x  -> return x
    Nothing -> error $ "No such block: " ++ show c

fresh :: Codegen Word
fresh = do
  i <- gets count
  modify $ \s -> s { count = 1 + i }
  return (i + 1)

compFunBody (Block name body) = execCodegen $ do
  enter <- addBlock (fromId name)
  _ <- setBlock enter
  body' <- mapM compInstr body
  blk <- current
  modifyBlock (blk { term = Just $ AST.Do $ AST.Ret (Just (last body')) [] })

compTy :: Language.Malgo.KNormal.Type -> T.Type
compTy (NameTy name) =
  case lookup name typeMap of
    Just x  -> x
    Nothing -> error $ show name ++ " (type) is not found."
  where
    typeMap = [ (Raw (Name "Int"), T.IntegerType 32)
              , (Raw (Name "Float"), T.FloatingPointType T.DoubleFP)
              , (Raw (Name "Bool"), T.IntegerType 1)
              , (Raw (Name "Char"), T.IntegerType 8)
              , (Raw (Name "String"), T.PointerType (T.IntegerType 8)
                                      (AddrSpace.AddrSpace 0))
              , (Raw (Name "Unit"), T.NamedTypeReference "unit")
              ]
compTy (FunTy (TupleTy xs) retTy) =
  let xs' = map compTy xs
      retTy' = compTy retTy
  in T.FunctionType retTy' xs' False

compTy x = error $ "unreachable: compTy " ++ show x

alloca ty name = instr (T.PointerType ty (AddrSpace.AddrSpace 0)) name $ AST.Alloca ty Nothing 0 []

store ptr val = justDo $ AST.Store False ptr val Nothing 0 []

load ty name ptr = instr ty name $ AST.Load False ptr Nothing 0 []

assign :: Name.Name -> AST.Operand -> Codegen ()
assign name ref = do
  st <- gets symtab
  modify $ \s -> s { symtab = (name, ref) : st }

getSym :: Name.Name -> Codegen AST.Operand
getSym name = do
  st <- gets symtab
  return $ case lookup name st of
    Just x  -> x
    Nothing -> error $ show name ++ " is not found. \n" ++ show st

instr :: T.Type -> Maybe Name.Name -> AST.Instruction -> Codegen AST.Operand
instr ty name ins = do
  ref <- case name of
    Just x  -> return x
    Nothing -> fmap Name.UnName fresh

  assign ref (AST.LocalReference ty ref)

  blk <- current
  let i = stack blk
  modifyBlock (blk { stack = (ref AST.:= ins) : i })
  return $ AST.LocalReference ty ref

justDo :: AST.Instruction -> Codegen ()
justDo ins = do
  blk <- current
  let i = stack blk
  modifyBlock (blk { stack = AST.Do ins : i })

-- compInstr :: Instr -> Codegen ()
compInstr ((name, ty), Int x) = do
  let ty' = compTy ty
  let name' = Name.Name (fromId name)
  a <- alloca ty' Nothing
  store a $ AST.ConstantOperand (C.Int 32 x)
  load ty' (Just name') a
compInstr ((name, ty), Bool x) = do
  let ty' = compTy ty
  let name' = Name.Name (fromId name)
  a <- alloca ty' Nothing
  store a $ AST.ConstantOperand (C.Int 1 (if x then 1 else 0))
  load ty' (Just name') a
-- compVal Unit = do
--   a <- alloca (T.NamedTypeReference "unit") Nothing
--   store a $ AST.ConstantOperand $ C.Undef (T.NamedTypeReference "unit")
--   load (T.NamedTypeReference "unit") a
compInstr ((name, ty), Unit) = do
  let ty' = compTy ty
  let name' = Name.Name (fromId name)
  a <- alloca (T.NamedTypeReference "unit") Nothing
  store a $ AST.ConstantOperand $ C.Undef (T.NamedTypeReference "unit")
  load ty' (Just name') a
compInstr ((name, ty), App fn args) = do
  let ty' = compTy ty
  let name' = Name.Name (fromId name)
  fn' <- compCallable fn
  args' <- mapM (getSym . Name.Name . fromId . fst) args
  call ty' (Just name') fn' args'

toArgs :: [AST.Operand] -> [(AST.Operand, [PA.ParameterAttribute])]
toArgs = map (\x -> (x, []))

call ty name fn args = instr ty name $ AST.Call Nothing CC.C [] (Right fn) (toArgs args) [] []

compCallable (name, typ) = do
  let typ' = T.PointerType (compTy typ) (AddrSpace.AddrSpace 0)
  return (AST.ConstantOperand (C.GlobalReference typ' (Name.Name (fromId name))))

-- -- compVal :: Val -> Codegen AST.Instruction
-- compVal (Int x)      = do
--   a <- alloca (T.IntegerType 32) Nothing
--   store a $ AST.ConstantOperand (C.Int 32 x)
--   load (T.IntegerType 32) a
-- compVal (Float x)    = do
--   a <- alloca (T.FloatingPointType T.DoubleFP) Nothing
--   store a $ AST.ConstantOperand (C.Float (F.Double x))
--   load (T.FloatingPointType T.DoubleFP) a
-- compVal (Bool True)  = do
--   a <- alloca (T.IntegerType 1) Nothing
--   store a $ AST.ConstantOperand (C.Int 1 1)
--   load (T.IntegerType 1) a
-- compVal (Bool False)  = do
--   a <- alloca (T.IntegerType 1) Nothing
--   store a $ AST.ConstantOperand (C.Int 1 0)
--   load (T.IntegerType 1) a

-- compVal (Char x)     = do
--   a <- alloca (T.IntegerType 8) Nothing
--   store a $ AST.ConstantOperand (C.Int 8 (toInteger (ord x)))
--   load (T.IntegerType 8) a
-- compVal (String x) = do
--   a <- alloca (T.PointerType (T.IntegerType 8) (AddrSpace.AddrSpace 0)) Nothing
--   store a $ AST.ConstantOperand $ C.Array (T.IntegerType 8) (map (C.Int 8 . toInteger . ord) x)
--   load (T.PointerType (T.IntegerType 8) (AddrSpace.AddrSpace 0)) a
