{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Language.Malgo.LLVM where

-- llvm-hs/llvm-hs-kaleidoscopeベース
import           Control.Monad.State
import qualified Data.ByteString                 as B
import qualified Data.ByteString.Short           as BS
import           Data.Char                       (chr, ord)
import           Data.Function
import           Data.List
import qualified Data.Map                        as Map
import           Data.Monoid
import           Data.String                     (IsString, fromString)

import           Language.Malgo.HIR              (Id (..))

-- import           Language.Malgo.HIR    (Id (..))
-- import qualified Language.Malgo.MIR    as MIR
-- import           Language.Malgo.Syntax (Type (..))
import qualified LLVM.AST                        as AST
import qualified LLVM.AST.AddrSpace              as AddrSpace
import qualified LLVM.AST.CallingConvention      as CC
import qualified LLVM.AST.Constant               as Constant
import qualified LLVM.AST.Float                  as Float
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST.Global                 as Global
import qualified LLVM.AST.IntegerPredicate       as IP
import qualified LLVM.AST.Linkage                as Linkage
import qualified LLVM.AST.Name                   as Name
import qualified LLVM.AST.ParameterAttribute     as PA
import qualified LLVM.AST.Type                   as Type

-----
-- Module
----

newtype LLVM a = LLVM (State AST.Module a)
  deriving (Functor, Applicative, Monad, MonadState AST.Module)

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM mod (LLVM m) = execState m mod

emptyModule :: BS.ShortByteString -> AST.Module
emptyModule label = AST.defaultModule { AST.moduleName = label }

addDefn :: AST.Definition -> LLVM ()
addDefn d = do
  defs <- gets AST.moduleDefinitions
  modify $ \s -> s { AST.moduleDefinitions = defs ++ [d] }

defineVar :: BS.ShortByteString -> Type.Type -> Constant.Constant -> LLVM ()
defineVar label typ val = addDefn $
  AST.GlobalDefinition AST.globalVariableDefaults
  { Global.name = Name.Name label
  , Global.type' = typ
  , Global.initializer = Just val
  }

defineFunc :: Type.Type -> BS.ShortByteString -> [(Type.Type, Name.Name)] -> Codegen a -> LLVM ()
defineFunc retTy label params body = addDefn $
  AST.GlobalDefinition  AST.functionDefaults
  { Global.name = Name.Name label
  , Global.parameters = ([Global.Parameter ty nm [] | (ty, nm) <- params], False)
  , Global.returnType = retTy
  , Global.basicBlocks = createBlocks $ execCodegen $ do
      enter <- addBlock entryBlockName
      _ <- setBlock enter
      body
  }

externalVar :: BS.ShortByteString -> Type.Type -> LLVM ()
externalVar label typ = addDefn $
  AST.GlobalDefinition AST.globalVariableDefaults
  { Global.name = Name.Name label
  , Global.linkage = Linkage.External
  , Global.type' = typ
  }

externalFunc
  :: Type.Type
     -> BS.ShortByteString -> [(Type.Type, Name.Name)] -> LLVM ()
externalFunc retTy label params = addDefn $
  AST.GlobalDefinition AST.functionDefaults
  { Global.name = Name.Name label
  , Global.linkage = Linkage.External
  , Global.parameters = ([Global.Parameter ty nm [] | (ty, nm) <- params], False) , Global.returnType = retTy
  , Global.basicBlocks = []
  }

-----
-- Types
-----

-- 32bit integer
intTy :: Type.Type
intTy = Type.IntegerType 32

-- IEEE 754 double
floatTy :: Type.Type
floatTy = Type.FloatingPointType Type.DoubleFP

-- 1bit integer(bool)
boolTy :: Type.Type
boolTy = Type.IntegerType 1

-- 8bit integer(char)
charTy :: Type.Type
charTy = Type.IntegerType 8

-- 8bit integer pointer(string)
stringTy :: Type.Type
stringTy = Type.PointerType charTy (AddrSpace.AddrSpace 0)

-- 1bit integer pointer(unit)
unitTy :: Type.Type
unitTy = Type.PointerType boolTy (AddrSpace.AddrSpace 0)

funTy :: Type.Type -> [Type.Type] -> Type.Type
funTy retTy argTys = Type.FunctionType retTy argTys False

-----
-- Name
-----
type Names = Map.Map BS.ShortByteString Int

uniqueName :: BS.ShortByteString -> Names -> (BS.ShortByteString, Names)
uniqueName nm ns =
  case Map.lookup nm ns of
    Nothing -> (nm,  Map.insert nm 1 ns)
    Just ix -> (nm <> fromString (show ix), Map.insert nm (ix+1) ns)

-----
-- Codegen State
-----
type SymbolTable = [(BS.ShortByteString, AST.Operand)]

data CodegenState
  = CodegenState {
    currentBlock :: Name.Name -- Name of the active block to append to
  , blocks       :: Map.Map Name.Name BlockState -- Blocks for function
  , blockCount   :: Int
  , count        :: Word -- Count of unnamed instruction
  , names        :: Names
  , symtab       :: SymbolTable
  } deriving Show

data BlockState
  = BlockState {
    idx   :: Int
  , stack :: [AST.Named AST.Instruction]
  , term  :: Maybe (AST.Named AST.Terminator)
  } deriving Show

-----
-- Codegen Oparations
-----

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

entryBlockName :: BS.ShortByteString
entryBlockName = "entry"

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

emptyCodegen :: CodegenState
emptyCodegen = CodegenState { currentBlock = Name.Name entryBlockName
                            , blocks = Map.empty
                            , blockCount = 1
                            , count = 0
                            , names = Map.empty
                            , symtab = []
                            }

execCodegen :: Codegen a -> CodegenState
execCodegen m = execState (runCodegen m) emptyCodegen

fresh :: Codegen Word
fresh = do
  i <- gets count
  modify $ \s -> s { count = 1 + i }
  return (i + 1)

instr :: Type.Type -> Maybe Name.Name -> AST.Instruction -> Codegen AST.Operand
instr ty name ins = do
  ref <- case name of
    Just x  -> return x
    Nothing -> fresh >>= return . Name.UnName
  -- n <- fresh
  -- let ref = Name.UnName n
  blk <- current
  let i = stack blk
  modifyBlock (blk { stack = (ref AST.:= ins) : i })
  return $ local ty ref

justDo :: AST.Instruction -> Codegen ()
justDo ins = do
  blk <- current
  let i = stack blk
  modifyBlock (blk { stack = AST.Do ins : i })

terminator :: AST.Named AST.Terminator -> Codegen ()
terminator trm = do
  blk <- current
  modifyBlock (blk { term = Just trm })
  -- return trm
  return ()

-----
-- Symbol Table
-----

assign :: BS.ShortByteString -> AST.Operand -> Codegen ()
assign var x = do
  lcls <- gets symtab
  modify $ \s -> s { symtab = [(var, x)] ++ lcls }

getvar :: BS.ShortByteString -> Codegen AST.Operand
getvar var = do
  syms <- gets symtab
  case lookup var syms of
    Just x  -> return x
    Nothing -> error $ "Local variable not in scope: " ++ show var

-----
-- Block Stack
-----

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

getBlock :: Codegen Name.Name
getBlock = gets currentBlock

current :: Codegen BlockState
current = do
  c <- gets currentBlock
  blks <- gets blocks
  case Map.lookup c blks of
    Just x  -> return x
    Nothing -> error $ "No such block: " ++ show c

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s { blocks = Map.insert active new (blocks s) }

local :: Type.Type -> Name.Name -> AST.Operand
local = AST.LocalReference

global :: Type.Type -> Name.Name -> Constant.Constant
global = Constant.GlobalReference

add :: AST.Operand -> AST.Operand -> Codegen AST.Operand
add a b = instr intTy Nothing $ AST.Add False False a b []

fadd :: AST.Operand -> AST.Operand -> Codegen AST.Operand
fadd a b = instr floatTy Nothing $ AST.FAdd AST.NoFastMathFlags a b []

sub :: AST.Operand -> AST.Operand -> Codegen AST.Operand
sub a b = instr intTy Nothing $ AST.Sub False False a b []

fsub :: AST.Operand -> AST.Operand -> Codegen AST.Operand
fsub a b = instr floatTy Nothing $ AST.FSub AST.NoFastMathFlags a b []

mul :: AST.Operand -> AST.Operand -> Codegen AST.Operand
mul a b = instr intTy Nothing $ AST.Mul False False a b []

fmul :: AST.Operand -> AST.Operand -> Codegen AST.Operand
fmul a b = instr floatTy Nothing $ AST.FMul AST.NoFastMathFlags a b []

sdiv :: AST.Operand -> AST.Operand -> Codegen AST.Operand
sdiv a b = instr intTy Nothing $ AST.SDiv False a b []

fdiv :: AST.Operand -> AST.Operand -> Codegen AST.Operand
fdiv a b = instr floatTy Nothing $ AST.FDiv AST.NoFastMathFlags a b []

srem :: AST.Operand -> AST.Operand -> Codegen AST.Operand
srem a b = instr intTy Nothing $ AST.SRem a b []

frem :: AST.Operand -> AST.Operand -> Codegen AST.Operand
frem a b = instr floatTy Nothing $ AST.FRem AST.NoFastMathFlags a b []

andb :: AST.Operand -> AST.Operand -> Codegen AST.Operand
andb a b = instr boolTy Nothing $ AST.And a b []

orb :: AST.Operand -> AST.Operand -> Codegen AST.Operand
orb a b = instr boolTy Nothing $ AST.Or a b []

icmp :: IP.IntegerPredicate -> AST.Operand -> AST.Operand -> Codegen AST.Operand
icmp cond a b = instr boolTy Nothing $ AST.ICmp cond a b []

fcmp :: FP.FloatingPointPredicate -> AST.Operand -> AST.Operand -> Codegen AST.Operand
fcmp cond a b = instr boolTy Nothing $ AST.FCmp cond a b []

cons :: Constant.Constant -> AST.Operand
cons = AST.ConstantOperand

toArgs :: [AST.Operand] -> [(AST.Operand, [PA.ParameterAttribute])]
toArgs = map (\x -> (x, []))

call :: Type.Type -> AST.Operand -> [AST.Operand] -> Codegen AST.Operand
call ty fn args = instr ty Nothing $ AST.Call Nothing CC.C [] (Right fn) (toArgs args) [] []

alloca :: Type.Type -> Maybe Name.Name -> Codegen AST.Operand
alloca ty name = instr ty name $ AST.Alloca ty Nothing 0 []

store :: AST.Operand -> AST.Operand -> Codegen ()
store ptr val = justDo $ AST.Store False ptr val Nothing 0 []

load :: Type.Type -> AST.Operand -> Codegen AST.Operand
load ty ptr = instr ty Nothing $ AST.Load False ptr Nothing 0 []

ret :: AST.Operand -> Codegen (AST.Named AST.Terminator)
ret val = terminator $ AST.Do $ AST.Ret (Just val) []

retvoid :: Codegen (AST.Named AST.Terminator)
retvoid = terminator $ AST.Do $ AST.Ret Nothing []

fromId :: IsString a => Id -> a
fromId (Sym s) = fromString s

str2Name = Name.Name . fromString

id2Name = Name.Name . fromId
