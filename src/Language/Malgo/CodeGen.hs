{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Language.Malgo.CodeGen where

import           Data.Char
import           Data.List                       (last)
import           Data.Maybe
import           Data.String
import qualified Data.Text                       as T

import qualified LLVM.AST
import qualified LLVM.AST.Constant               as C
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST.IntegerPredicate       as IP
import           LLVM.AST.Operand
import qualified LLVM.AST.Type                   as LT
import qualified LLVM.AST.Typed                  as LT
import           LLVM.IRBuilder                  as IRBuilder

import           Language.Malgo.HIR              (Op (..))
import           Language.Malgo.MIR
import           Language.Malgo.Prelude          hiding (bit)
import           Language.Malgo.Rename           (ID (..))
import qualified Language.Malgo.Type             as T
import           Language.Malgo.TypeCheck        (TypedID (..))

data GenState = GenState { _table    :: Map TypedID Operand
                         , _term     :: Operand -> GenExpr () -- if式の際の最終分岐先などに利用
                         , _internal :: Map String Operand
                         }

type GenExpr a = IRBuilderT GenDec a
type GenDec = ModuleBuilderT (State GenState)

addTable ::
  (MonadState GenState m, MonadTrans t) =>
  TypedID -> Operand -> t m ()
addTable name opr =
  lift (modify (\s -> s { _table = insert name opr (_table s)}))

addInternal ::
  (MonadState GenState m, MonadTrans t) =>
  String -> Operand -> t m ()
addInternal name opr =
  lift (modify (\s -> s { _internal = insert name opr (_internal s) }))

convertType :: T.Type -> LT.Type
convertType "Int"          = LT.i32
convertType "Float"        = LT.double
convertType "Bool"         = LT.i1
convertType "Char"         = LT.i8
convertType "String"       = LT.ptr LT.i8
convertType "Unit"         =
  LT.StructureType False []
convertType (T.NameTy x) = panic $ "unknown type: " <> show x
convertType (T.TupleTy xs) =
  LT.ptr $ LT.StructureType False (map convertType xs)
convertType (T.FunTy params retTy) =
  LT.FunctionType (convertType retTy) (map convertType params ++ [LT.ptr LT.i8]) False
convertType (T.ClsTy params retty) =
  LT.ptr $ LT.StructureType False
  [ LT.ptr $ LT.FunctionType (convertType retty) (map convertType params ++ [LT.ptr LT.i8]) False
  , LT.ptr LT.i8
  ]

sizeof :: MonadIRBuilder m => LT.Type -> m Operand
sizeof ty = do
  nullptr <- pure $ ConstantOperand (C.Null (LT.ptr ty))
  ptr <- gep nullptr [ConstantOperand (C.Int 32 1)]
  ptrtoint ptr LT.i64

getRef ::
  (MonadState GenState m, Monad (t m), MonadTrans t) =>
  TypedID -> t m Operand
getRef i = do
  m <- lift (gets _table)
  case lookup i m of
    Nothing -> panic $ show i <> " is not found in " <> show m
    Just x  -> pure x

term :: IRBuilderT GenDec Operand -> IRBuilderT GenDec ()
term o = do
  t <- lift (gets _term)
  o' <- o
  t o'

fromTypedID :: IsString a => TypedID -> a
fromTypedID (TypedID i _) =
  fromString $ show (_name i) <> "zi" <> show (_uniq i)

char :: Applicative f => Integer -> f Operand
char = pure . ConstantOperand . C.Int 8

gcMalloc ::
  (MonadIRBuilder (t m), MonadState GenState m, MonadTrans t) =>
  Operand -> t m Operand
gcMalloc bytesOpr = do
  f <- lift (fromJust . lookup "GC_malloc" <$> gets _internal)
  call f [(bytesOpr, [])]

gcInit :: IRBuilderT GenDec Operand
gcInit = do
  f <- lift (fromJust . lookup "GC_init" <$> gets _internal)
  call f []

captureStruct :: [TypedID] -> LT.Type
captureStruct xs =
  LT.StructureType False (map (convertType . _type) xs)

genExpr :: Expr TypedID -> IRBuilderT GenDec ()
genExpr e@(Var _)   = term (genExpr' e) `named` "var"
genExpr e@(Int _)   = term (genExpr' e) `named` "int"
genExpr e@(Float _) = term (genExpr' e) `named` "float"
genExpr e@(Bool _)  = term (genExpr' e) `named` "bool"
genExpr e@(Char _)  = term (genExpr' e) `named` "char"
genExpr e@(String _) = term (genExpr' e) `named` "string"
genExpr e@Unit = term (genExpr' e) `named` "unit"
genExpr e@(Tuple _) = term (genExpr' e) `named` "tuple"
genExpr e@(TupleAccess _ _) = term (genExpr' e) `named` "tuple_access"
genExpr e@(CallCls _ _) = term (genExpr' e) `named` "callcls"
genExpr e@(CallDir _ _) = term (genExpr' e) `named` "calldir"
genExpr e@(Let (ValDec _ _) _) = term (genExpr' e) `named` "valdec"
genExpr e@(Let ClsDec{} _) = term (genExpr' e) `named` "clsdec"
genExpr e@If{} = term (genExpr' e) `named` "if"
genExpr e@(BinOp op _ _) = term (genExpr' e) `named` fromString (show op)

genExpr' :: Expr TypedID -> IRBuilderT GenDec Operand
genExpr' (Var a)    = getRef a `named` "var"
genExpr' (Int i)    = int32 i `named` "int"
genExpr' (Float d)  = double d `named` "float"
genExpr' (Bool b)   = bit (if b then 1 else 0) `named` "bool"
genExpr' (Char c)   = char (toInteger . ord $ c) `named` "char"
genExpr' (String xs) = do
  p <- gcMalloc (ConstantOperand $ C.Int 64 $ toInteger $ T.length xs + 1) `named` "string"
  mapM_ (uncurry $ addChar p) (zip [0..] $ T.unpack xs <> ['\0'])
  pure p
  where addChar p i c = do
          i' <- int32 i
          p' <- gep p [i'] `named` "tmp_char"
          c' <- char (toInteger . ord $ c)
          store p' 0 c'
genExpr' Unit = pure (ConstantOperand $ C.Undef (convertType "Unit")) `named` "unit"
genExpr' e@(Tuple xs) = do
  size <- sizeof (convertType $ T.typeOf e) `named` "tuple_size"
  p <- (\p -> bitcast p (convertType (T.typeOf e))) =<< gcMalloc size `named` "tuple_ptr"
  forM_ (zip [0..] xs) $ \(i, x) -> do
    p' <- gep p [ ConstantOperand (C.Int 32 0)
                , ConstantOperand (C.Int 32 i)
                ] `named` "tuple_elem_ptr"
    o <- getRef x
    store p' 0 o
  pure p
genExpr' (TupleAccess x i) = do
  x' <- getRef x
  p <- gep x' [ ConstantOperand (C.Int 32 0)
              , ConstantOperand (C.Int 32 (toInteger i))
              ] `named` "tuple_elem_ptr"
  load p 0
genExpr' (CallDir fn args) = do
  fn' <- getRef fn
  args' <- mapM (\a -> do a' <- getRef a; pure (a', [])) args
  call fn' (args' ++ [(ConstantOperand (C.Undef $ LT.ptr LT.i8), [])])
genExpr' (CallCls cls args) = do
  cls' <- getRef cls
  fnptr <- gep cls' [ ConstantOperand (C.Int 32 0)
                    , ConstantOperand (C.Int 32 0)
                    ] `named` "fn_ptr"
  capptr <- gep cls' [ ConstantOperand (C.Int 32 0)
                     , ConstantOperand (C.Int 32 1)
                     ] `named` "cap_ptr"
  args' <- mapM (\a -> do a' <- getRef a; pure (a', [])) args
  fn <- load fnptr 0
  cap <- load capptr 0
  call fn (args' ++ [(cap, [])])
genExpr' (Let (ValDec name val) e) = do
  val' <- genExpr' val `named` (fromString . show $ pretty name)
  addTable name val'
  genExpr' e
genExpr' (Let (ClsDec name fn captures) e) = do
  size <- sizeof (captureStruct captures) `named` "captures_size"
  p <- flip bitcast (LT.ptr $ captureStruct captures) =<< gcMalloc size `named` "captures_ptr"
  fn' <- getRef fn
  let fn'ty = LT.typeOf fn'
  fn'size <- sizeof fn'ty `named` "fn_size"
  let capty = LT.ptr LT.i8
  cap <- bitcast p capty `named` "cap_ptr"
  capSize <- sizeof capty `named` "cap_size"
  clsSize <- add fn'size capSize `named` "cls_size"
  clsptr <- flip bitcast (LT.ptr $ LT.StructureType False [fn'ty, capty]) =<< gcMalloc clsSize `named` "cls_ptr"
  fnp <- gep clsptr [ ConstantOperand (C.Int 32 0)
                    , ConstantOperand (C.Int 32 0)
                    ] `named` "fn_ptr"
  store fnp 0 fn'
  capp <- gep clsptr [ ConstantOperand (C.Int 32 0)
                     , ConstantOperand (C.Int 32 1)
                     ] `named` "cap_ptr"
  store capp 0 cap
  addTable name clsptr
  forM_ (zip [0..] captures) $ \(i, x) -> do
    p' <- gep p [ ConstantOperand (C.Int 32 0)
                , ConstantOperand (C.Int 32 i)
                ] `named` "capture_ptr"
    o <- getRef x
    store p' 0 o
  genExpr' e
genExpr' (If c t f) = do
  c' <- getRef c
  r <- alloca (convertType (T.typeOf t)) Nothing 0 `named` "resultptr"
  (end, t', f') <- (,,) <$> freshName "end" <*> freshName "then" <*> freshName "else"
  condBr  c' t' f'
  backup <- lift (gets _term)
  lift (modify $ \s -> s { _term = \o -> store r 0 o >> br end })
  emitBlockStart t'; genExpr t
  emitBlockStart f'; genExpr f
  lift (modify $ \s -> s { _term = backup })
  emitBlockStart end
  load r 0
genExpr' (BinOp op x y) = do
  let op' = case op of
        Add      -> add
        Sub      -> sub
        Mul      -> mul
        Div      -> sdiv
        FAdd     -> fadd
        FSub     -> fsub
        FMul     -> fmul
        FDiv     -> fdiv
        Mod      -> frem
        Eq ty -> case ty of
                   "Int"   -> icmp IP.EQ
                   "Float" -> fcmp FP.OEQ
                   "Bool"  -> icmp IP.EQ
                   "Char"  -> icmp IP.EQ
                   _       -> panic $ show ty <> " is not comparable"
        Neq ty -> case ty of
                   "Int"   -> icmp IP.NE
                   "Float" -> fcmp FP.ONE
                   "Bool"  -> icmp IP.NE
                   "Char"  -> icmp IP.NE
                   _       -> panic $ show ty <> " is not comparable"
        Lt ty -> case ty of
                   "Int"   -> icmp IP.SLT
                   "Float" -> fcmp FP.OLT
                   "Bool"  -> icmp IP.SLT
                   "Char"  -> icmp IP.SLT
                   _       -> panic $ show ty <> " is not comparable"
        Gt ty -> case ty of
                   "Int"   -> icmp IP.SGT
                   "Float" -> fcmp FP.OGT
                   "Bool"  -> icmp IP.SGT
                   "Char"  -> icmp IP.SGT
                   _       -> panic $ show ty <> " is not comparable"
        Le ty -> case ty of
                   "Int"   -> icmp IP.SLE
                   "Float" -> fcmp FP.OLE
                   "Bool"  -> icmp IP.SLE
                   "Char"  -> icmp IP.SLE
                   _       -> panic $ show ty <> " is not comparable"
        Ge ty -> case ty of
                   "Int"   -> icmp IP.SGE
                   "Float" -> fcmp FP.OGE
                   "Bool"  -> icmp IP.SGE
                   "Char"  -> icmp IP.SGE
                   _       -> panic $ show ty <> " is not comparable"
        And -> IRBuilder.and
        Or -> IRBuilder.or
  x' <- getRef x
  y' <- getRef y
  op' x' y'

genExDec ::
  (MonadTrans t, MonadState GenState m, MonadModuleBuilder (t m)) =>
  ExDec TypedID -> t m ()
genExDec (ExDec name str) = do
  let (argtys, retty) = case T.typeOf name of
                          (T.FunTy p r) -> (map convertType p ++ [LT.ptr LT.i8], convertType r)
                          _ -> panic $ show name <> " is not callable"
  o <- extern (fromString $ toS str) argtys retty
  addTable name o

genFunDec :: FunDec TypedID -> GenDec ()
genFunDec (FunDec fn@(TypedID _ (T.FunTy _ retty)) params captures body) = do
  let fn' = fromString (show (pretty fn))
  let params' = map (\(TypedID name ty) ->
                       (convertType ty, fromString (show (pretty name))))
                params
                ++ [(LT.ptr LT.i8, fromString "captures")]
  let retty' = convertType retty
  backup <- gets _table
  let body' xs = do
        mapM_ (uncurry addTable) (zip params xs)
        unless (null captures) $ do
          capPtr <- bitcast (last xs) (LT.ptr $ captureStruct captures) `named` "capturesPtr"
          forM_ (zip [0..] captures) $ \(i, c) -> do
            p' <- gep capPtr [ ConstantOperand (C.Int 32 0)
                             , ConstantOperand (C.Int 32 i)
                             ] `named` fromString (show (pretty c) ++ "Ptr")
            o <- load p' 0 `named` fromString (show (pretty c))
            addTable c o
        genExpr body
  _ <- function fn' params' retty' body'
  modify $ \e -> e { _table = backup }
genFunDec x = panic (show $ pretty x <> " is not valid")

genMain :: Expr TypedID -> GenDec ()
genMain e = do
  _ <- function "main" [] (convertType "Int")
       (\_ ->
          do _ <- gcInit
             _ <- genExpr' e `named` "main"
             i <- int32 0
             ret i)
  pure ()

genProgram ::
  Program TypedID -> GenDec ()
genProgram (Program fs es body) = do
  addInternal "GC_malloc" =<< extern (fromString "GC_malloc") [LT.i64] (LT.ptr LT.i8)
  addInternal "GC_init" =<< extern (fromString "GC_init") [] LT.void
  mapM_ genExDec es
  mapM_ addFunction fs
  mapM_ genFunDec fs
  _ <- genMain body
  pure ()
  where
    addFunction (FunDec fn@(TypedID _ (T.FunTy params retty)) _ _ _) = do
      let fnop = ConstantOperand $ C.GlobalReference (LT.FunctionType (convertType retty) (map convertType params ++ [LT.ptr LT.i8]) False) (fromString $ show $ pretty fn)
      addTable fn fnop
    addFunction x = panic (show $ pretty x <> " is not valid")

dumpCodeGen ::
  ModuleBuilderT (StateT GenState Identity) a
  -> [LLVM.AST.Definition]
dumpCodeGen m =
  flip evalState (GenState mempty ret mempty) $ execModuleBuilderT emptyModuleBuilder m
