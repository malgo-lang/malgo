module Koriel.Core.CodeGen.PrintLLVM (codeGen) where

import Control.Lens (ifor, ifor_, view, _1)
import Control.Monad.Cont (ContT, runContT, withContT)
import Control.Monad.Extra (whileM)
import Data.Foldable (maximum)
import Data.HashMap.Strict qualified as HashMap
import Data.String.Conversions (convertString)
import Data.Text qualified as Text
import Data.Tuple.Extra (uncurry3)
import Koriel.Core.Syntax hiding (variable)
import Koriel.Core.Type
import Koriel.Id (Id (..), ModuleName (..), idToText)
import Koriel.Prelude
import Koriel.Pretty

type KName = Id Type

-- Note: LLVM IR is not probably newline sensitive.
-- So we don't care about newlines. (except for readability)

-- | Generate and Write LLVM IR to file from a Koriel module.
codeGen ::
  -- | Source path
  FilePath ->
  -- | Destination path
  FilePath ->
  ModuleName ->
  Program KName ->
  IO ()
codeGen srcPath dstPath moduleName Program {..} = do
  withFile dstPath WriteMode \output -> do
    let interned =
          foldr buildInternTable mempty topVars
            & \m -> foldr buildInternTable m topFuns
    register <- newIORef mempty
    postprocess <- newIORef []
    runReaderT ?? CodeGenEnv {..} $ do
      putAsmLn $ "source_filename = \"" <> toText srcPath <> "\""
      extern "GC_init" [] VoidT

      for_ extFuns \case
        (name, ps :-> r) -> extern name ps r
        _ -> error "invalid type"

      traverse_ (uncurry3 variable) topVars

      traverse_ (uncurry4 function) topFuns

      resetRegister
      define "main" Int32T do
        for_ topVars \case
          (name, typ, expr)
            | toAsm typ == "ptr" -> do
                name <- lookupName name
                eOpr <- putExprLn expr
                store typ eOpr name
            | otherwise -> pure ()
        pure "0"

      whileM do
        postprocess' <- readIORef postprocess
        writeIORef postprocess []
        void $ sequence_ postprocess'
        not . null <$> readIORef postprocess
  where
    buildInternTable s = HashMap.insert (view _1 s) Global
    uncurry4 f (w, x, y, z) = f w x y z

type BlockBuilder m a = ContT () m a

runBlockBuilder :: BlockBuilder m a -> (a -> m ()) -> m ()
runBlockBuilder = runContT

withTerminator :: BlockBuilder m Register -> ((Register -> m ()) -> Register -> m ()) -> BlockBuilder m Register
withTerminator = flip withContT

-- | Register name it don't have type information.
-- e.g. "%1", "@a"
-- @Register@ also be used as a constant.
-- e.g. "42", "3.14"
type Register = Text

putExprLn ::
  HasCallStack =>
  (MonadReader CodeGenEnv m, MonadIO m) =>
  Expr KName ->
  -- | Register name of the result
  BlockBuilder m Register
putExprLn (Atom atom) = atomToConstant atom
putExprLn (Call f xs) = do
  f' <- atomToConstant f
  xs' <- traverse atomToConstant xs

  captureAddr <- gep (typeOf f) f' [0, 0]
  capture <- load AnyT captureAddr

  funcAddr <- gep (typeOf f) f' [0, 1]
  func <- load (typeOf f) funcAddr -- typeOf f == "ptr"
  callClosure func (typeOf f) capture xs'
putExprLn (CallDirect f xs) = do
  f' <- lookupName f
  xs' <- traverse atomToConstant xs
  call f' (typeOf f) xs'
putExprLn (RawCall name (params :-> ret) xs) = do
  xs' <- traverse atomToConstant xs
  call (sigil Global <> show name) (params :-> ret) xs'
putExprLn BinOp {} = error "not implemented"
putExprLn (Cast ty a) = do
  a' <- atomToConstant a
  register "cast" $ do
    putAsmLn $ "bitcast " <> toAsm (typeOf a) <> " " <> a' <> " to " <> toAsm ty
putExprLn (Let ds e) = do
  modifier <- prepare ds
  local modifier do
    traverse_ putLocalDefLn ds
    putExprLn e
  where
    -- intern and allocate all variables
    prepare [] = pure identity
    prepare (LocalDef {_variable, typ} : rest) = do
      (modifier, reg) <- intern _variable Local
      putAsmLn $ reg <> " = alloca " <> toInnerType typ
      modifier' <- prepare rest
      pure $ modifier' . modifier
putExprLn _ = do
  (putAsmLn "; not implemented" >> pure "%undefined") `withTerminator` \_ _ -> putAsmLn "unreachable"

putLocalDefLn :: (MonadReader CodeGenEnv m, MonadIO m) => LocalDef KName -> BlockBuilder m ()
putLocalDefLn (LocalDef closureName (_ :-> ret) (Fun params body)) = do
  -- Reserve generation of closure internal function
  let internalFunctionName = closureName.name <> ".closure"
  defer do
    resetRegister
    let captureRegister = sigil Local <> closureName.name <> ".capture"
    (modifier, params') <- prepare params
    local modifier do
      putAsmLn $ "define internal " <> toAsm ret <> " @" <> internalFunctionName <> "(" <> Text.intercalate ", " (("ptr " <> captureRegister) : params') <> ") {"
      entry <- label "entry"
      putAsmLn $ entry <> ":"
      runBlockBuilder ?? (\opr -> putAsmLn $ "ret " <> toAsm ret <> " " <> opr) $ do
        -- unpack capture
        modifier <- unpackCapture captureRegister
        local modifier $
          putExprLn body
      putAsmLn "}"
  capture <- register "capture" $ putAsmLn $ "alloca " <> captureType
  ifor_ fvs $ \i fv -> do
    fv' <- lookupName fv
    addr <- register "addr" $ putAsmLn $ "getelementptr " <> captureType <> ", ptr " <> capture <> ", " <> "i32 0, i32 " <> show i
    store (typeOf fv) fv' addr
  closure <- lookupName closureName
  whereCapture <- gep (typeOf closureName) closure [0, 0]
  store AnyT capture whereCapture
  whereFunction <- gep (typeOf closureName) closure [0, 1]
  store AnyT (sigil Global <> internalFunctionName) whereFunction
  where
    prepare [] = pure (identity, [])
    prepare (p : ps) = do
      (modifier, reg) <- intern p Local
      (modifier', regs) <- prepare ps
      pure (modifier' . modifier, toAsm (typeOf p) <> " " <> reg : regs)
    fvs = toList $ freevars (Fun params body)
    captureType = "{ " <> Text.intercalate ", " (map (toAsm . typeOf) fvs) <> " }"
    unpackCapture captureAddr = do
      newEnv <-
        HashMap.fromList <$> ifor fvs \i fv -> do
          fvAddr <- register "fvAddr" $ putAsmLn $ "getelementptr " <> captureType <> ", ptr " <> captureAddr <> ", " <> "i32 0, i32 " <> show i
          putAsmLn $ sigil Local <> show (idToText fv) <> " = load " <> toAsm (typeOf fv) <> ", ptr " <> fvAddr
          pure (fv, Local)
      pure $ \env -> env {interned = newEnv <> env.interned}
putLocalDefLn LocalDef {_variable} = putAsmLn $ "; " <> show _variable

-- TODO: Use apply function like `stgApply` to prevent code explosion
callClosure ::
  (MonadReader CodeGenEnv m, MonadIO m) =>
  -- | Function address
  Register ->
  -- | Function type
  Type ->
  -- | Capture
  Register ->
  -- | Arguments
  [Register] ->
  BlockBuilder m Register
callClosure func (params :-> ret) capture args = do
  let args' = zipWith (\p a -> toAsm p <> " " <> a) params args
  register "callClosure" $ do
    putAsmLn $ "call " <> toAsm ret <> " " <> func <> "(" <> Text.intercalate ", " ("ptr " <> capture : args') <> ")"
callClosure _ t _ _ = errorDoc $ sep ["invalid type in callClosure:", pPrint t]

call ::
  (MonadReader CodeGenEnv m, MonadIO m) =>
  -- | Function address
  Register ->
  -- | Function type
  Type ->
  -- | Arguments
  [Register] ->
  BlockBuilder m Register
call func (params :-> ret) args = do
  let args' = zipWith (\p a -> toAsm p <> " " <> a) params args
  register "call" $ do
    putAsmLn $ "call " <> toAsm ret <> " " <> func <> "(" <> Text.intercalate ", " args' <> ")"
call _ t _ = errorDoc $ sep ["invalid type in call:", pPrint t]

load ::
  (MonadReader CodeGenEnv m, MonadIO m) =>
  -- | Type of the value
  Type ->
  -- | Address of the value
  Register ->
  BlockBuilder m Register
load typ address = register "load" $ do
  putAsmLn $ "load " <> toAsm typ <> ", ptr " <> address

store ::
  (MonadReader CodeGenEnv m, MonadIO m) =>
  -- | Type of the value
  Type ->
  -- | Value
  Register ->
  -- | Address of the value
  Register ->
  BlockBuilder m ()
store typ value address = do
  putAsmLn $ "store " <> toAsm typ <> " " <> value <> ", ptr " <> address

gep :: (MonadReader CodeGenEnv m, MonadIO m) => Type -> Register -> [Int] -> BlockBuilder m Register
gep typ address index = do
  register "gep" $ do
    putAsmLn $
      "getelementptr "
        <> toInnerType typ
        <> ", "
        <> toAsm typ
        <> " "
        <> address
        <> ", "
        <> Text.intercalate "," (map (\i -> "i32 " <> show i :: Text) index)

toInnerType :: Type -> Text
toInnerType (_ :-> _) = "{ ptr, ptr }"
toInnerType StringT = "i8"
toInnerType (SumT cs) =
  let size = maximum $ sizeOfCon <$> toList cs
   in "{ i8, " <> if size == (0 :: Int) then "{} }" else "<" <> show size <> " x " <> "i8>" <> " }"
  where
    sizeOfCon (Con _ ts) = sum $ sizeOfType <$> ts
    sizeOfType (_ :-> _) = 8
    sizeOfType Int32T = 4
    sizeOfType Int64T = 8
    sizeOfType FloatT = 4
    sizeOfType DoubleT = 8
    sizeOfType CharT = 1
    sizeOfType StringT = 8
    sizeOfType BoolT = 1
    sizeOfType (SumT _) = 8
    sizeOfType (PtrT _) = 8
    sizeOfType (RecordT _) = 8
    sizeOfType AnyT = 8
    sizeOfType VoidT = 0
toInnerType (PtrT ty) = toAsm ty
toInnerType AnyT = "i8"
toInnerType t = errorDoc $ sep ["invalid type in toInnerType:", pPrint t]

variable ::
  (MonadReader CodeGenEnv m, MonadIO m) =>
  KName ->
  Type ->
  Expr KName ->
  m ()
variable name _ expr = do
  resetRegister
  name <- lookupName name
  putAsm $ name <> " = "
  putGlobalValue expr
  putAsmLn ""
  where
    putAsm asm = do
      CodeGenEnv {output} <- ask
      liftIO $ hPutText output asm

function ::
  (MonadReader CodeGenEnv m, MonadIO m) =>
  KName ->
  [KName] ->
  Type ->
  Expr KName ->
  m ()
function name params (_ :-> ret) expr = do
  resetRegister
  name' <- lookupName name
  register <- newIORef mempty
  (modifier, params') <- prepare params
  local (modifier . \env -> env {register = register}) do
    putAsmLn $ "define " <> toAsm ret <> " " <> name' <> "(" <> Text.intercalate ", " params' <> ") {"
    entry <- label "entry"
    putAsmLn $ entry <> ":"
    runBlockBuilder (putExprLn expr) (\opr -> putAsmLn $ "ret " <> toAsm ret <> " " <> opr)
    putAsmLn "}"
  where
    -- intern all parameters
    prepare :: (MonadReader CodeGenEnv m, MonadIO m) => [KName] -> m (CodeGenEnv -> CodeGenEnv, [Text])
    prepare [] = pure (identity, [])
    prepare (p : ps) = do
      (modifier, p') <- intern p Local
      local modifier do
        (modifier', ps') <- prepare ps
        pure (modifier' . modifier, toAsm (typeOf p) <> " " <> p' : ps')
function name t _ _ = errorDoc $ sep ["invalid type in function:", pPrint name, pPrint t]

putGlobalValue ::
  (MonadReader CodeGenEnv m, MonadIO m) =>
  Expr KName ->
  m ()
putGlobalValue (Atom atom) = do
  atom' <- atomToConstant atom
  putAsmLn $ "global " <> toAsm (typeOf atom) <> " " <> atom'
putGlobalValue e | toAsm (typeOf e) == "ptr" = putAsmLn "global ptr undef"
putGlobalValue e = errorDoc $ sep ["cannot be global:", pPrint e]

atomToConstant :: HasCallStack => MonadReader CodeGenEnv m => Atom KName -> m Text
atomToConstant (Var name) = lookupName name
atomToConstant (Unboxed (Int32 i)) = pure $ show i
atomToConstant (Unboxed (Int64 i)) = pure $ show i
atomToConstant (Unboxed (Float f)) = pure $ show f
atomToConstant (Unboxed (Double d)) = pure $ show d
atomToConstant (Unboxed (Char c)) = pure $ show (ord c)
atomToConstant (Unboxed (String s)) =
  pure $
    "getelementptr inbounds ptr, "
      <> "["
      <> show (Text.length s + 1)
      <> " x i8], "
      <> "["
      <> Text.intercalate ", " (map (\c -> "i8 " <> show (ord c)) (convertString s))
      <> "i8 0], i64 0, i64 0"
atomToConstant (Unboxed (Bool b)) = pure $ show (fromEnum b)

data NameSpace = Global | Local

instance Pretty NameSpace where
  pPrint Global = "global"
  pPrint Local = "local"

data CodeGenEnv = CodeGenEnv
  { moduleName :: ModuleName,
    interned :: HashMap KName NameSpace,
    register :: IORef (HashMap Text Int),
    postprocess :: IORef [ReaderT CodeGenEnv IO ()],
    output :: Handle
  }

-- * Assembly builder

-- We don't define 'putAsm' in top-level because we want to simplify the problem about newlines.
-- If a function needs to put a assembly without newline, it can use 'output' directly.
putAsmLn :: (MonadReader CodeGenEnv m, MonadIO m) => Text -> m ()
putAsmLn asm = do
  CodeGenEnv {output} <- ask
  liftIO $ hPutTextLn output asm

class ToAsm a where
  -- | `toAsm` converts a value to LLVM IR.
  -- These values must be clearly convertible on their own.
  toAsm :: a -> Text

instance ToAsm Type where
  toAsm :: Type -> Text
  toAsm (_ :-> _) = "ptr"
  toAsm Int32T = "i32"
  toAsm Int64T = "i64"
  toAsm FloatT = "float"
  toAsm DoubleT = "double"
  toAsm CharT = "i8"
  toAsm StringT = "ptr"
  toAsm BoolT = "i1"
  toAsm (SumT _) = "ptr"
  toAsm (PtrT _) = "ptr"
  toAsm (RecordT _) = "ptr"
  toAsm AnyT = "ptr"
  toAsm VoidT = "void"

-- * Utilities

intern :: MonadReader CodeGenEnv m => KName -> NameSpace -> m (CodeGenEnv -> CodeGenEnv, Text)
intern name given = do
  CodeGenEnv {interned} <- ask
  case HashMap.lookup name interned of
    Just defined ->
      errorDoc $ sep ["duplicated name:", pPrint name, ",", "defined scope:", pPrint defined, ",", "given scope:", pPrint given]
    Nothing ->
      pure (\env -> env {interned = HashMap.insert name given interned}, sigil given <> show (idToText name))

sigil :: NameSpace -> Text
sigil Global = "@"
sigil Local = "%"

lookupName :: HasCallStack => MonadReader CodeGenEnv m => KName -> m Register
lookupName name = do
  CodeGenEnv {interned} <- ask
  case HashMap.lookup name interned of
    Just namespace -> pure $ sigil namespace <> show (idToText name)
    Nothing -> errorDoc $ "unbound variable: " <> pPrint name

register :: (MonadReader CodeGenEnv m, MonadIO m) => Text -> m () -> BlockBuilder m Register
register hint value = do
  reg <- newRegister hint
  putAsm $ reg <> " = "
  lift value
  pure reg
  where
    putAsm asm = do
      CodeGenEnv {output} <- ask
      liftIO $ hPutText output asm
    newRegister hint = do
      CodeGenEnv {register} <- ask
      register' <- readIORef register
      let count = HashMap.lookupDefault 0 hint register'
      modifyIORef register $ HashMap.insert hint (count + 1)
      pure $ sigil Local <> show (hint <> show count)

resetRegister :: (MonadReader CodeGenEnv m, MonadIO m) => m ()
resetRegister = do
  CodeGenEnv {register} <- ask
  writeIORef register HashMap.empty

label :: (MonadReader CodeGenEnv m, MonadIO m) => Text -> m Text
label hint = do
  CodeGenEnv {register} <- ask
  register' <- readIORef register
  let count = HashMap.lookupDefault 0 hint register'
  modifyIORef register $ HashMap.insert hint (count + 1)
  pure $ show (hint <> show count)

extern :: (MonadReader CodeGenEnv m, MonadIO m) => Text -> [Type] -> Type -> m ()
extern name params result = do
  putAsmLn $ "declare " <> toAsm result <> " @" <> name <> "(" <> Text.intercalate ", " (map toAsm params) <> ")"

define :: (MonadReader CodeGenEnv m, MonadIO m) => Text -> Type -> BlockBuilder m Text -> m ()
define name retType body = do
  putAsmLn $ "define " <> toAsm retType <> " @" <> name <> "() {"
  void $ runBlockBuilder body (\opr -> putAsmLn $ "ret " <> toAsm retType <> " " <> opr)
  putAsmLn "}"

defer :: (MonadReader CodeGenEnv m, MonadIO m) => ReaderT CodeGenEnv IO () -> m ()
defer action = do
  CodeGenEnv {postprocess} <- ask
  modifyIORef postprocess (action :)