{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Malgo.IRBuilder where

-- This module is very hacky and unsupported.
-- This is based on https://github.com/llvm-hs/llvm-irbuilder.
-- If you can use llvm-irbuilder, please use it.

-- 言語に依存しないLLVM IRのビルダー
-- Stateモナドをベースとする

import           Control.Monad.State
import           Data.ByteString.Short           as BS
import           Data.Monoid
import           Data.String

import qualified Language.Malgo.Utils            as U

import           LLVM.AST
import qualified LLVM.AST                        as AST
import qualified LLVM.AST.CallingConvention      as CC
import           LLVM.AST.Constant               as C
import qualified LLVM.AST.Constant               as C
import qualified LLVM.AST.FloatingPointPredicate as FP
import           LLVM.AST.Global                 as Global
import qualified LLVM.AST.IntegerPredicate       as IP
import           LLVM.AST.ParameterAttribute

newtype ModuleBuilder a = ModuleBuilder
  { unModuleBuilder :: StateT ModuleBuilderState (Either String) a}
  deriving (Functor, Applicative, Monad, MonadState ModuleBuilderState)

data ModuleBuilderState = ModuleBuilderState
  { _defs         :: [Definition] }
  deriving Show

runModuleBuilder
  :: ModuleBuilderState
     -> ModuleBuilder t -> Either String (t, [Definition])
runModuleBuilder s (ModuleBuilder m) = do
  (a, mbs) <- runStateT m s
  let ds = reverse . _defs $ mbs
  return (a, ds)

execModuleBuilder
  :: ModuleBuilderState
     -> ModuleBuilder a -> Either String [Definition]
execModuleBuilder s m = snd <$> runModuleBuilder s m

emptyModuleBuilder :: ModuleBuilderState
emptyModuleBuilder = ModuleBuilderState
  { _defs = [] }

emitDefn :: Definition -> ModuleBuilder ()
emitDefn def = modify $ \s -> s { _defs = def : _defs s}

defun
  :: Name -- ^ Function name
  -> [(Type, Maybe ShortByteString)] -- ^ Parameter
  -> Type -- ^ Return type
  -> ([Operand] -> InstrBuilder ()) -- ^ Function body builder. This takes a parameters list.
  -> ModuleBuilder Operand -- ^ defun returns a GlobalReference
defun label params retty body = do
  let tys = fmap fst params
  let body' = runInstrBuilder emptyInstrBuilder $ do
        paramNames <- forM params $ \(_, mname) ->
          maybe fresh (fresh `named`) mname
        body $ zipWith LocalReference tys paramNames
        return paramNames
  (paramNames, blocks) <- case body' of
    Left x       -> ModuleBuilder $ lift . Left $ x
    Right (p, b) -> return (p, b)
  let def = GlobalDefinition functionDefaults
            { name = label
            , parameters = ([Parameter ty nm [] | ty <- tys, nm <- paramNames], False)
            , returnType = retty
            , basicBlocks = blocks
            }
      funty = FunctionType retty tys False
  emitDefn def
  return $ ConstantOperand $ C.GlobalReference funty label

defvar
  :: Name -- ^ Variable name
  -> Type
  -> Constant
  -> ModuleBuilder Operand
defvar label ty val = do
  let def = GlobalDefinition globalVariableDefaults
            { Global.name = label
            , Global.type' = ty
            , Global.initializer = Just val
            }
  emitDefn def
  return $ ConstantOperand $ C.GlobalReference ty label

deftype :: Name -> Type -> ModuleBuilder ()
deftype label ty = do
  let def = TypeDefinition label (Just ty)
  emitDefn def

newtype InstrBuilder a = InstrBuilder
  { unInstrBuilder :: StateT InstrBuilderState (Either String) a }
  deriving (Functor, Applicative, Monad, MonadState InstrBuilderState)

data InstrBuilderState = InstrBuilderState
  { _supply         :: Word
  , _usedNames      :: [ShortByteString]
  , _nameSuggestion :: Maybe ShortByteString
  , _blocks         :: [BasicBlock]
  , _block          :: Maybe PartialBlock
  }

emptyInstrBuilder :: InstrBuilderState
emptyInstrBuilder = InstrBuilderState
  { _supply = 0
  , _usedNames = []
  , _nameSuggestion = Nothing
  , _blocks = []
  , _block = Nothing
  }

-- 構築中のBasicBlock
data PartialBlock = PartialBlock
  { _partialBlockName :: Name
  , _instrs           :: [Named Instruction]
  , _term             :: Maybe (Named Terminator)
  }

emptyPartialBlock :: Name -> PartialBlock
emptyPartialBlock nm = PartialBlock nm [] Nothing

runInstrBuilder
  :: InstrBuilderState
     -> InstrBuilder t -> Either String (t, [BasicBlock])
runInstrBuilder s (InstrBuilder m) = do
  (a, ibs) <- runStateT m s
  let blks = reverse . _blocks $ ibs
  return (a, blks)

execInstrBuilder
  :: InstrBuilderState
     -> InstrBuilder a -> Either String [BasicBlock]
execInstrBuilder s m = snd <$> runInstrBuilder s m

modifyBlock :: (PartialBlock -> PartialBlock) -> InstrBuilder ()
modifyBlock f = do
  mblk <- gets _block
  case mblk of
    Nothing -> do
      nm <- freshUnName
      modify $ \s -> s { _block = Just (f $ emptyPartialBlock nm) }
    Just blk -> modify $ \s -> s { _block = Just (f blk) }


freshUnName :: InstrBuilder Name
freshUnName = do
  n <- gets _supply
  modify $ \s -> s { _supply = 1 + n }
  return $ UnName n

fresh :: InstrBuilder Name
fresh = do
  mhint <- gets _nameSuggestion
  case mhint of
    Nothing -> freshUnName
    Just hint -> do
      used <- gets _usedNames
      let candidates = hint : [hint <> fromString (show n) | n <- [(1::Int)..]]
          (unusedName:_) = filter (not . (`elem` used)) candidates
      modify $ \s -> s { _usedNames = unusedName : used}
      return $ Name unusedName

emitInstr
  :: Type -- ^ Return type
  -> Instruction
  -> InstrBuilder Operand
emitInstr retty instr = do
  nm <- fresh
  modifyBlock $ \b -> b
    { _instrs = (nm := instr) : _instrs b }
  return (LocalReference retty nm)

emitTerm :: Terminator -> InstrBuilder ()
emitTerm term = modifyBlock $ \b -> b
  { _term = Just (Do term) }

block :: InstrBuilder Name
block = do
  mb <- gets _block
  case mb of
    Nothing -> return ()
    Just b -> do
      let instrs = reverse (_instrs b)
          newBlock = case _term b of
            Nothing -> BasicBlock (_partialBlockName b) instrs (Do (Ret Nothing []))
            Just term -> BasicBlock (_partialBlockName b) instrs term
      modify $ \s -> s
        { _blocks = newBlock : _blocks s}
  nm <- fresh
  modify $ \s -> s { _block = Just $ emptyPartialBlock nm }
  return nm

named :: InstrBuilder r -> ShortByteString -> InstrBuilder r
named ir name = do
  before <- gets _nameSuggestion
  modify $ \s -> s { _nameSuggestion = Just name }
  result <- ir
  modify $ \s -> s { _nameSuggestion = before }
  return result
