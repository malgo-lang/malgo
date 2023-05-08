module Koriel.Core.CodeGen.PrintLLVM (codeGen) where

import Control.Lens (over, view, _1)
import Control.Monad.Cont (ContT, runContT, withContT)
import Data.Foldable (maximum)
import Data.HashMap.Strict qualified as HashMap
import Data.String.Conversions (convertString)
import Data.Text qualified as Text
import Data.Tuple.Extra (uncurry3)
import GHC.Float (castDoubleToWord64, castFloatToWord32)
import Koriel.Core.Syntax hiding (variable)
import Koriel.Core.Type
import Koriel.Id (Id (..), IdSort (Temporal), ModuleName (..), idToText)
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
    runReaderT ?? CodeGenEnv {..} $ do
      putAsmLn $ "source_filename = \"" <> toText srcPath <> "\""
      extern "GC_init" [] VoidT

      for_ extFuns \case
        (name, ps :-> r) -> extern name ps r
        _ -> error "invalid type"

      traverse_ (uncurry3 variable) topVars

      traverse_ (uncurry4 function) topFuns

      define "main" Int32T do
        for_ topVars \case
          (name, typ, expr)
            | toAsm typ == "ptr" -> do
                name <- lookupName name
                eOpr <- putExprLn expr
                putAsmLn $ "store ptr " <> eOpr <> ", ptr " <> name
            | otherwise -> pure ()
        pure "0"
  where
    buildInternTable s = HashMap.insert (view _1 s) Global
    uncurry4 f (w, x, y, z) = f w x y z

type BlockBuilder m a = ContT () m a

runBlockBuilder :: BlockBuilder m a -> (a -> m ()) -> m ()
runBlockBuilder = runContT

withTerminator :: BlockBuilder m Register -> ((Register -> m ()) -> Register -> m ()) -> BlockBuilder m Register
withTerminator = flip withContT

type Register = Text

putExprLn ::
  (MonadReader CodeGenEnv m, MonadIO m) =>
  Expr KName ->
  -- | Register name of the result
  BlockBuilder m Register
putExprLn (Atom atom) = do
  addr <- register "atom" $ putAsmLn $ "alloca " <> toAsm (typeOf atom)
  value <- atomToAsm atom
  store (typeOf atom) value addr
  reg <- load (typeOf atom) addr
  pure reg
putExprLn (Call f xs) = do
  f' <- register "f" $ putAsmLn =<< atomToAsm f
  xs' <- traverse (register "x" . putAsmLn <=< atomToAsm) xs

  captureAddr <- gep (typeOf f) f' [0, 0]
  capture <- load AnyT captureAddr

  funcAddr <- gep (typeOf f) f' [0, 1]
  func <- load (typeOf f) funcAddr -- typeOf f == "ptr"
  callClosure func (typeOf f) capture xs'
putExprLn _ = do
  (putAsmLn "; not implemented" >> pure "%undefined") `withTerminator` \_ _ -> putAsmLn "unreachable"

callClosure ::
  (MonadReader CodeGenEnv m, MonadIO m) =>
  -- \| Function address
  Register ->
  -- | Function type
  Type ->
  -- | Capture
  Register ->
  -- | Arguments
  [Register] ->
  ContT () m Register
callClosure func (params :-> ret) capture args = do
  let args' = zipWith (\p a -> toAsm p <> " " <> a) params args
  register "callClosure" $ do
    putAsmLn $ "call " <> toAsm ret <> " " <> func <> "(" <> Text.intercalate ", " ("ptr " <> capture : args') <> ")"
callClosure _ t _ _ = errorDoc $ sep ["invalid type in callClosure:", pPrint t]

load ::
  (MonadReader CodeGenEnv m, MonadIO m) =>
  -- | Type of the value
  Type ->
  -- | Address of the value
  Register ->
  ContT () m Register
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
  ContT () m ()
store typ value address = do
  putAsmLn $ "store " <> toAsm typ <> " " <> value <> ", ptr " <> address

gep :: (MonadReader CodeGenEnv m, MonadIO m) => Type -> Register -> [Int] -> ContT () m Register
gep typ address index = do
  register "gep" $ do
    putAsmLn $
      "getelementptr "
        <> toInnerType typ
        <> ", "
        <> toAsm typ
        <> " "
        <> address
        <> " "
        <> "["
        <> Text.intercalate "," (map (\i -> "i32 " <> show i :: Text) index)
        <> "]"
  where
    toInnerType (_ :-> _) = "{ ptr, ptr }"
    toInnerType StringT = "i8"
    toInnerType (SumT cs) =
      let size = maximum $ sizeOfCon <$> toList cs
       in "{ i8, " <> if size == (0 :: Int) then "{}" else "<" <> show size <> " x " <> "i8>" <> " }"
    toInnerType (PtrT ty) = toAsm ty
    toInnerType AnyT = "i8"
    toInnerType t = errorDoc $ sep ["invalid type in toInnerType:", pPrint t]
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

variable ::
  (MonadReader CodeGenEnv m, MonadIO m) =>
  KName ->
  Type ->
  Expr KName ->
  m ()
variable name _ expr = do
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
  atom <- atomToAsm atom
  putAsmLn $ "global " <> atom
putGlobalValue e | toAsm (typeOf e) == "ptr" = putAsmLn "global ptr undef"
putGlobalValue e = errorDoc $ sep ["cannot be global:", pPrint e]

atomToAsm ::
  MonadReader CodeGenEnv m =>
  Atom KName ->
  m Text
atomToAsm (Var name) = lookupName name
atomToAsm (Unboxed (Int32 i)) = pure $ "i32 " <> show i
atomToAsm (Unboxed (Int64 i)) = pure $ "i64 " <> show i
atomToAsm (Unboxed (Float f)) = pure $ "bitcast i32 " <> show (castFloatToWord32 f) <> " to float"
atomToAsm (Unboxed (Double d)) = pure $ "bitcast i64 " <> show (castDoubleToWord64 d) <> " to double"
atomToAsm (Unboxed (Char c)) = pure $ "i8 " <> show (ord c)
atomToAsm (Unboxed (String s)) = do
  pure $
    "getelementptr inbounds ptr, "
      <> "["
      <> show (Text.length s + 1)
      <> " x i8], "
      <> "["
      <> Text.intercalate ", " (map (\c -> "i8 " <> show (ord c)) (convertString s))
      <> "i8 0], i64 0, i64 0"
atomToAsm (Unboxed (Bool b)) = pure $ "i1 " <> show (fromEnum b)

data NameSpace = Global | Local

instance Pretty NameSpace where
  pPrint Global = "global"
  pPrint Local = "local"

data CodeGenEnv = CodeGenEnv
  { moduleName :: ModuleName,
    interned :: HashMap KName NameSpace,
    register :: IORef (HashMap Text Int),
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

lookupName :: MonadReader CodeGenEnv m => KName -> m Text
lookupName name = do
  CodeGenEnv {interned} <- ask
  case HashMap.lookup name interned of
    Just namespace -> pure $ sigil namespace <> show (idToText name)
    Nothing -> errorDoc $ "unbound variable: " <> pPrint name

register :: (MonadReader CodeGenEnv m, MonadIO m) => Text -> m () -> BlockBuilder m Text
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
