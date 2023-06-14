{-# LANGUAGE QuasiQuotes #-}

module Koriel.Core.CodeGen.PrintLLVM (codeGen) where

import Control.Exception (assert)
import Control.Lens (ifor_, view, _1)
import Control.Monad.Cont (ContT, runContT, withContT)
import Control.Monad.Extra (whileM)
import Data.Foldable (maximum)
import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Data.String.Conversions (convertString)
import Data.Text qualified as Text
import Data.Tuple.Extra (uncurry3)
import Koriel.Core.Syntax hiding (variable)
import Koriel.Core.Type
import Koriel.Id (Id (..), ModuleName (..), idToText)
import Koriel.Prelude
import Koriel.Pretty
import Relude.Unsafe qualified as Unsafe
import Text.RawString.QQ (r)
import Text.Regex.TDFA ((=~))

type KName = Id Type

validRegisterPattern :: Text
validRegisterPattern = "^[-a-zA-Z$._][-a-zA-Z$._0-9]*$"

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
    stringCount <- newIORef 0
    postprocess <- newIORef []
    runReaderT ?? CodeGenEnv {..} $ do
      putAsmLn $ "source_filename = \"" <> toText srcPath <> "\""
      extern "GC_init" [] VoidT
      extern "malgo_malloc" [Int64T] AnyT
      extern "malgo_hash_table_new" [] AnyT
      extern "malgo_hash_table_insert" [AnyT, StringT, AnyT] VoidT

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
    buildInternTable s = HashMap.insert (view _1 s) (sigil Global <> toAsm (view _1 s))
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
  (MonadReader CodeGenEnv m, MonadIO m) =>
  Expr KName ->
  -- | Register name of the result
  BlockBuilder m Register
putExprLn (Atom atom) = atomToConstant atom
putExprLn (Call f xs) = do
  f' <- atomToConstant f
  xs' <- traverse atomToConstant xs

  captureAddr <- register "captureAddr" $ gep (typeOf f) f' [0, 0]
  capture <- register "capture" $ load AnyT captureAddr

  funcAddr <- register "funcAddr" $ gep (typeOf f) f' [0, 1]
  func <- register "func" $ load (typeOf f) funcAddr -- typeOf f == "ptr"
  register "call" $ callClosure func (typeOf f) capture xs'
putExprLn (CallDirect f xs) = do
  f' <- lookupName f
  xs' <- traverse atomToConstant xs
  register "calldirect" $ call f' (typeOf f) xs'
putExprLn (RawCall name (params :-> ret) xs) = do
  xs' <- traverse atomToConstant xs
  if name =~ validRegisterPattern
    then register "rawcall" $ call (sigil Global <> name) (params :-> ret) xs'
    else error $ "invalid function name: " <> show name
putExprLn BinOp {} = error "not implemented"
putExprLn (Cast ty a)
  | toAsm ty /= toAsm (typeOf a) = do
      a' <- atomToConstant a
      register "cast" $ do
        putAsmLn $ "bitcast " <> toAsm (typeOf a) <> " " <> a' <> " to " <> toAsm ty
  | otherwise = do
      a' <- atomToConstant a
      putAsmLn $ "; bitcast " <> toAsm (typeOf a) <> " " <> a' <> " to " <> toAsm ty
      pure a'
putExprLn (Let ds e) = do
  modifier <- prepare ds
  local modifier do
    traverse_
      ( \d -> do
          addr <- lookupName d._variable
          putLocalDefLn addr d
      )
      ds
    putExprLn e
  where
    -- intern and allocate all variables
    prepare [] = pure identity
    prepare (LocalDef {_variable, _object = Record {}} : rest) = do
      (modifier, reg) <- intern _variable Local
      putAsm (reg <> " = ") >> putAsmLn "call ptr @malgo_hash_table_new()"
      modifier' <- prepare rest
      pure $ modifier' . modifier
    prepare (LocalDef {_variable, typ} : rest) = do
      (modifier, reg) <- intern _variable Local
      putAsm (reg <> " = ") >> mallocType (toInnerType typ)
      modifier' <- prepare rest
      pure $ modifier' . modifier
    putAsm asm = do
      CodeGenEnv {output} <- ask
      liftIO $ hPutText output asm
putExprLn (Assign _ v e) | typeOf v == VoidT = do
  _ <- putExprLn v
  putExprLn e
putExprLn (Assign x v e) = do
  v' <- putExprLn v
  local (\env -> env {interned = HashMap.insert x v' env.interned}) do
    putExprLn e
putExprLn _ = do
  (putAsmLn "; not implemented" >> pure "%undefined") `withTerminator` \_ _ -> putAsmLn "unreachable"

putLocalDefLn :: (MonadReader CodeGenEnv m, MonadIO m) => Register -> LocalDef KName -> BlockBuilder m ()
putLocalDefLn addr (LocalDef closureName (_ :-> ret) (Fun params body)) = do
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
        local modifier
          $ putExprLn body
      putAsmLn "}"
  capture <- register "capture" $ mallocType captureType
  ifor_ fvs $ \i fv -> do
    fv' <- lookupName fv
    addr <- register "addr" $ putAsmLn $ "getelementptr " <> captureType <> ", ptr " <> capture <> ", " <> "i32 0, i32 " <> show i
    store (typeOf fv) fv' addr
  whereCapture <- register "whereCapture" $ gep (typeOf closureName) addr [0, 0]
  store AnyT capture whereCapture
  whereFunction <- register "whereFunction" $ gep (typeOf closureName) addr [0, 1]
  store AnyT (sigil Global <> internalFunctionName) whereFunction
  where
    prepare [] = pure (identity, [])
    prepare (p : ps) = do
      (modifier, reg) <- intern p Local
      (modifier', regs) <- prepare ps
      pure (modifier' . modifier, toAsm (typeOf p) <> " " <> reg : regs)
    fvs = toList $ freevars (Fun params body)
    captureType = "{ " <> Text.intercalate ", " (map (toAsm . typeOf) fvs) <> " }"
    unpackCapture :: (MonadReader CodeGenEnv m, MonadIO m) => Register -> BlockBuilder m (CodeGenEnv -> CodeGenEnv)
    unpackCapture captureAddr = do
      loadField captureAddr (zip [0 ..] fvs)
    loadField :: (MonadReader CodeGenEnv m, MonadIO m) => Register -> [(Int, KName)] -> BlockBuilder m (CodeGenEnv -> CodeGenEnv)
    loadField captureAddr ((i, fv) : rest) = do
      (modifier, reg) <- intern fv Local
      fvAddr <- register "fvAddr" $ putAsmLn $ "getelementptr " <> captureType <> ", ptr " <> captureAddr <> ", " <> "i32 0, i32 " <> show i
      putAsmLn $ reg <> " = load " <> toAsm (typeOf fv) <> ", ptr " <> fvAddr
      local modifier $ do
        modifier' <- loadField captureAddr rest
        pure $ modifier' . modifier
    loadField _ [] = pure identity
putLocalDefLn _ (LocalDef name typ Fun {}) = do
  errorDoc $ "invalid function type: " <> pPrint typ <> " for " <> pPrint name
putLocalDefLn addr (LocalDef (typeOf -> SumT cs) _ (Pack _ con@(Con _ ts) xs)) = do
  let trueType = "{ i8, {" <> Text.intercalate "," (map toAsm ts) <> "} }"
  tagAddr <- register "tagAddr" $ gep' trueType addr [0, 0]
  store CharT (show $ Unsafe.fromJust $ List.elemIndex con cs) tagAddr
  ifor_ xs \i x -> do
    x' <- atomToConstant x
    fieldAddr <- register "fieldAddr" $ gep' trueType addr [0, 1, i]
    store (ts Unsafe.!! i) x' fieldAddr
putLocalDefLn _ (LocalDef name _ Pack {}) = do
  errorDoc $ "invalid pack type: " <> pPrint (typeOf name) <> " for " <> pPrint name
putLocalDefLn addr (LocalDef _ _ (Record kvs)) = do
  for_ (HashMap.toList kvs) \(k, v) -> do
    k' <- atomToConstant (Unboxed $ String k)
    v' <- atomToConstant v
    call (sigil Global <> "malgo_hash_table_insert") ([AnyT, StringT, AnyT] :-> VoidT) [addr, k', v']

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
  m ()
callClosure func (params :-> ret) capture args = do
  let args' = zipWith (\p a -> toAsm p <> " " <> a) params args
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
  m ()
call func (params :-> ret) args = do
  let args' = zipWith (\p a -> toAsm p <> " " <> a) params args
  putAsmLn $ "call " <> toAsm ret <> " " <> func <> "(" <> Text.intercalate ", " args' <> ")"
call _ t _ = errorDoc $ sep ["invalid type in call:", pPrint t]

mallocBytes ::
  (MonadReader CodeGenEnv m, MonadIO m) =>
  -- | Size
  Register ->
  m ()
mallocBytes size =
  putAsmLn $ "call ptr @malgo_malloc(i64 " <> size <> ")"

mallocType ::
  (MonadReader CodeGenEnv m, MonadIO m) =>
  -- | Type
  Text ->
  m ()
mallocType typ = mallocBytes $ sizeOf typ
  where
    sizeOf typ = "ptrtoint (ptr getelementptr (" <> typ <> ", ptr null, i32 1) to i64)"

load ::
  (MonadReader CodeGenEnv m, MonadIO m) =>
  -- | Type of the value
  Type ->
  -- | Address of the value
  Register ->
  m ()
load typ address =
  putAsmLn $ "load " <> toAsm typ <> ", ptr " <> address

store ::
  (MonadReader CodeGenEnv m, MonadIO m) =>
  -- | Type of the value
  Type ->
  -- | Value
  Register ->
  -- | Address of the value
  Register ->
  m ()
store typ value address = do
  putAsmLn $ "store " <> toAsm typ <> " " <> value <> ", ptr " <> address

alloca :: (MonadReader CodeGenEnv m, MonadIO m) => Type -> m ()
alloca typ = putAsmLn $ "alloca " <> toInnerType typ

gep :: (MonadReader CodeGenEnv m, MonadIO m) => Type -> Register -> [Int] -> m ()
gep typ address index = do
  putAsmLn
    $ "getelementptr "
    <> toInnerType typ
    <> ", ptr "
    <> address
    <> ", "
    <> Text.intercalate "," (map (\i -> "i32 " <> show i :: Text) index)

gep' :: (MonadReader CodeGenEnv m, MonadIO m) => Text -> Text -> [Int] -> m ()
gep' typ address index = do
  putAsmLn
    $ "getelementptr "
    <> typ
    <> ", ptr "
    <> address
    <> ", "
    <> Text.intercalate "," (map (\i -> "i32 " <> show i :: Text) index)

toInnerType :: Type -> Text
toInnerType (_ :-> _) = "{ ptr, ptr }"
toInnerType StringT = "i8"
toInnerType (SumT cs) =
  let size = maximum $ sizeOfCon <$> toList cs
   in -- We use i8 array to represent payload of sum type.
      -- Because a specific struct type for each sum type is not required when allocating memory.
      -- But we need to know the maximum size of the payload.
      "{ i8, " <> if size == (0 :: Int) then "{} }" else "<" <> show size <> " x " <> "i8>" <> " }"
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

atomToConstant :: (HasCallStack, MonadIO m) => MonadReader CodeGenEnv m => Atom KName -> m Text
atomToConstant (Var name) = lookupName name
atomToConstant (Unboxed (Int32 i)) = pure $ show i
atomToConstant (Unboxed (Int64 i)) = pure $ show i
atomToConstant (Unboxed (Float f)) = pure $ show f
atomToConstant (Unboxed (Double d)) = pure $ show d
atomToConstant (Unboxed (Char c)) = pure $ show (ord c)
-- TODO: add global variable and return its name
atomToConstant (Unboxed (String s)) = do
  -- Generate global variable identifier
  sName <- newStringName
  defer $ putAsmLn $ sName <> " = constant [" <> show (Text.length s + 1) <> " x i8] c\"" <> s <> "\\00\""
  pure sName
-- Generate global variable storing the string
atomToConstant (Unboxed (Bool b)) = pure $ show (fromEnum b)

data NameSpace = Global | Local

instance Pretty NameSpace where
  pPrint Global = "global"
  pPrint Local = "local"

data CodeGenEnv = CodeGenEnv
  { moduleName :: ModuleName,
    interned :: HashMap KName Register,
    register :: IORef (HashMap Text Int),
    stringCount :: IORef Int,
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

instance ToAsm KName where
  toAsm :: KName -> Text
  toAsm = show . idToText

-- * Utilities

intern :: MonadReader CodeGenEnv m => KName -> NameSpace -> m (CodeGenEnv -> CodeGenEnv, Text)
intern name given = do
  CodeGenEnv {interned} <- ask
  let register = newName (HashMap.lookup name interned)
  pure (\env -> env {interned = HashMap.insert name register interned}, register)
  where
    -- Generate a new register name based on the interned name.
    newName :: Maybe Register -> Register
    newName Nothing = sigil given <> toAsm name
    -- Add a '$' to the end of the name to avoid name conflict.
    -- '$' used as the prefix of temporal (mean generated by compiler) identifiers in Koriel, so any identifier in Koriel cannot end with '$'.
    newName (Just defined) = sigil given <> Text.dropEnd 1 (Text.drop 1 defined) <> "$\""

sigil :: NameSpace -> Text
sigil Global = "@"
sigil Local = "%"

-- TODO: If the register are global (head register == '@'), we should load the value and return it.
lookupName :: HasCallStack => MonadReader CodeGenEnv m => KName -> m Register
lookupName name = do
  CodeGenEnv {interned} <- ask
  case HashMap.lookup name interned of
    Just register -> pure register
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

newStringName :: (MonadReader CodeGenEnv m, MonadIO m) => m Text
newStringName = do
  CodeGenEnv {stringCount} <- ask
  count <- readIORef stringCount
  modifyIORef stringCount (+ 1)
  pure $ "@str" <> show count

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
