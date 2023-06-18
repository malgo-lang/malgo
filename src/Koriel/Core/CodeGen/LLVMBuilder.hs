module Koriel.Core.CodeGen.LLVMBuilder (
  CodeGenM,
  CodeGenEnv (..),
  runCodeGen,
  as,
  BlockBuilderT,
  runBlockBuilderT,
  Ident (..),
  Constant (..),
  NameSpace (..),
  constant,
  ToAsm (..),
  putAsmLn,
  call,
  callClosure,
  bitcast,
  getelementptr,
  load,
  store,
  mallocBytes,
  mallocType,
  resetRegister,
  globalVariable,
  ret,
  function,
  newString,
  generateAsByteString,
) where

import Control.Lens (traverseOf, traversed, _1)
import Control.Monad.Cont
import Data.HashMap.Strict qualified as HashMap
import Data.Knob qualified as Knob
import Data.Text qualified as Text
import Koriel.Prelude
import Koriel.Pretty
import Text.Regex.TDFA ((=~))

-- * Monads

type CodeGenM = ReaderT CodeGenEnv IO

data CodeGenEnv = CodeGenEnv
  { register :: IORef (HashMap Text Int),
    hint :: Text,
    stringCount :: IORef Int,
    postprocess :: IORef [CodeGenM ()],
    output :: Handle
  }

newtype CodeGen a = CodeGen (ReaderT CodeGenEnv IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO)

runCodeGen :: CodeGen a -> Handle -> IO a
runCodeGen (CodeGen m) output = do
  register <- newIORef mempty
  stringCount <- newIORef 0
  postprocess <- newIORef []
  let env = CodeGenEnv {register, hint = "x", stringCount, postprocess, output}
  runReaderT m env

generateAsByteString :: CodeGen () -> IO Text
generateAsByteString m = do
  knob <- Knob.newKnob ""
  Knob.withFileHandle knob "knob_in_LLVMBuilder" WriteMode $ \h -> do
    runCodeGen m h
    hFlush h
  bytes <- Knob.getContents knob
  pure $ decodeUtf8 bytes

as :: MonadReader CodeGenEnv m => m a -> Text -> m a
as m hint = local (\env -> env {hint}) m

newtype BlockBuilderT m a = BlockBuilderT (ContT () m a)
  deriving newtype (Functor, Applicative, Monad, MonadTrans, MonadState s, MonadReader r, MonadIO)

runBlockBuilderT :: BlockBuilderT m Constant -> (Constant -> m ()) -> m ()
runBlockBuilderT (BlockBuilderT m) = runContT m

class ToAsm a where
  -- | `toAsm` converts a value to LLVM IR.
  -- These values must be clearly convertible on their own.
  toAsm :: a -> Text

-- * LLVM objects

data Type
  = -- | e.g. "i32", "{ i32, i32 }"
    SimpleType Text
  | -- | e.g. "i32 (i32, i32)*" (After LLVM 16, function pointer is just "ptr". But we need to keep the structure of the function type.)
    FunctionType Type [Type]

instance Pretty Type where
  pPrint (SimpleType t) = pPrint t
  pPrint (FunctionType ret args) = pPrint ret <+> parens (hsep $ punctuate comma $ pPrint <$> args)

instance ToAsm Type where
  toAsm (SimpleType t) = t
  toAsm FunctionType {} = "ptr"

-- | Register name it don't have type information.
-- e.g. "%1", "@a"
data Ident = Ident {nameSpace :: NameSpace, name :: Text}

instance ToAsm Ident where
  toAsm Ident {..} = sigil nameSpace <> escapeRegisterName name

escapeRegisterName :: Text -> Text
escapeRegisterName name
  | isValidRegister name = name
  | '\"' `Text.elem` name = error "Invalid register name: double quotes can't be used in register name."
  | otherwise = "\"" <> name <> "\""

-- | e.g. "42", "3.14"
-- @Register@ also be used as a constant.
data Constant = Value Text | Register Ident

instance ToAsm Constant where
  toAsm (Value v) = v
  toAsm (Register v) = toAsm v

data NameSpace = Global | Local

instance Pretty NameSpace where
  pPrint Global = "global"
  pPrint Local = "local"

sigil :: NameSpace -> Text
sigil Global = "@"
sigil Local = "%"

-- | A valid register name pattern.
-- See: https://llvm.org/docs/LangRef.html#identifiers
-- Above rule can be escaped by double quotes.
validRegisterPattern :: Text
validRegisterPattern = "^([-a-zA-Z$._][-a-zA-Z$._0-9]*)|\"[^\"]*\"$"

-- | Check if the given string is a valid register name.
isValidRegister :: Text -> Bool
isValidRegister = (=~ validRegisterPattern)

-- | Create a constant from a value.
-- Values must be able to be represented as a string.
-- Conversion to a string is done by @show@.
constant :: Show a => a -> Constant
constant v = Value $ show v

bind :: (MonadReader CodeGenEnv m, MonadIO m) => Text -> m () -> m Ident
bind hint putValue = do
  reg <- newRegister Local hint
  putAsm $ toAsm reg <> " = "
  void putValue
  pure reg

newRegister :: (MonadReader CodeGenEnv m, MonadIO m) => NameSpace -> Text -> m Ident
newRegister ns hint = do
  CodeGenEnv {register} <- ask
  register' <- readIORef register
  let count = HashMap.lookupDefault 0 hint register'
  modifyIORef register $ HashMap.insert hint (count + 1)
  pure $ Ident ns (hint <> show count)

autoRegister :: (MonadReader CodeGenEnv m, MonadIO m) => m () -> m Ident
autoRegister putValue = do
  CodeGenEnv {hint} <- ask
  hint `bind` putValue

label :: (MonadReader CodeGenEnv m, MonadIO m) => Text -> m Text
label hint = do
  CodeGenEnv {register} <- ask
  register' <- readIORef register
  let count = HashMap.lookupDefault 0 hint register'
  modifyIORef register $ HashMap.insert hint (count + 1)
  pure $ escapeRegisterName (hint <> show count)

-- We don't define 'putAsm' in top-level because we want to simplify the problem about newlines.
-- If a function needs to put a assembly without newline, it can use 'output' directly.
putAsmLn :: (MonadReader CodeGenEnv m, MonadIO m) => Text -> m ()
putAsmLn asm = do
  CodeGenEnv {output} <- ask
  liftIO $ hPutTextLn output asm

putAsm :: (MonadReader CodeGenEnv m, MonadIO m) => Text -> m ()
putAsm asm = do
  CodeGenEnv {output} <- ask
  liftIO $ hPutText output asm

resetRegister :: (MonadReader CodeGenEnv m, MonadIO m) => m ()
resetRegister = do
  CodeGenEnv {register} <- ask
  writeIORef register HashMap.empty

globalVariable :: (MonadReader CodeGenEnv m, MonadIO m) => Ident -> Type -> Constant -> m ()
globalVariable name typ constant = do
  putAsmLn $ toAsm name <> " = global " <> toAsm typ <> " " <> toAsm constant

globalString :: (MonadReader CodeGenEnv m, MonadIO m) => Ident -> Text -> m ()
globalString name str = do
  putAsmLn $ toAsm name <> " = private unnamed_addr constant [" <> show (Text.length str + 1) <> " x i8] c\"" <> str <> "\\00\""

function ::
  (MonadReader CodeGenEnv m, MonadIO m) =>
  -- | Linkage
  Text ->
  -- | Function name
  Ident ->
  -- | Parameters
  [(Text, Type)] ->
  -- | Return type
  Type ->
  -- | Function body
  ([Ident] -> BlockBuilderT m Constant) ->
  m ()
function linkage name params retType expr = do
  resetRegister
  params <- traverseOf (traversed . _1) (newRegister Local) params
  putAsmLn $ "define " <> linkage <> " " <> toAsm retType <> " " <> toAsm name <> "(" <> Text.intercalate ", " (toAsmAsParam <$> params) <> ") {"
  entry <- label "entry"
  putAsmLn $ entry <> ":"
  runBlockBuilderT (expr $ map fst params) (ret $ toAsm retType)
  putAsmLn "}"
  where
    toAsmAsParam :: (Ident, Type) -> Text
    toAsmAsParam (name, typ) = toAsm typ <> " " <> toAsm name

defer :: (MonadReader CodeGenEnv m, MonadIO m) => CodeGenM () -> m ()
defer action = do
  CodeGenEnv {postprocess} <- ask
  modifyIORef postprocess (action :)

-- * Instructions

-- | Create a string constant.
-- The string is stored in a global variable.
-- The name of the variable is returned.
newString :: (MonadReader CodeGenEnv m, MonadIO m) => Text -> m Ident
newString str = do
  name <- newStringName
  defer $ globalString (Ident Global name) str
  pure $ Ident Global name
  where
    newStringName = do
      CodeGenEnv {stringCount} <- ask
      count <- readIORef stringCount
      modifyIORef stringCount (+ 1)
      pure $ "@str" <> show count

ret :: (MonadReader CodeGenEnv m, MonadIO m) => Text -> Constant -> m ()
ret typ opr = putAsmLn $ "ret " <> typ <> " " <> toAsm opr

call ::
  (MonadReader CodeGenEnv m, MonadIO m) =>
  -- | Function address
  Constant ->
  -- | Function type
  Type ->
  -- | Arguments
  [Constant] ->
  m Ident
call func (FunctionType ret params) args =
  autoRegister do
    let args' = zipWith (\p a -> toAsm p <> " " <> toAsm a) params args
    putAsmLn $ "call " <> toAsm ret <> " " <> toAsm func <> "(" <> Text.intercalate ", " args' <> ")"
call _ t _ = errorDoc $ sep ["invalid type in call:", pPrint t]

-- TODO: Use apply function like `stgApply` to prevent code explosion
callClosure ::
  (MonadReader CodeGenEnv m, MonadIO m) =>
  -- | Function address
  Constant ->
  -- | Function type
  Type ->
  -- | Capture
  Constant ->
  -- | Arguments
  [Constant] ->
  m Ident
callClosure func (FunctionType ret params) capture args = autoRegister do
  let args' = zipWith (\p a -> toAsm p <> " " <> toAsm a) params args
  putAsmLn $ "call " <> toAsm ret <> " " <> toAsm func <> "(" <> Text.intercalate ", " ("ptr " <> toAsm capture : args') <> ")"
callClosure _ t _ _ = errorDoc $ sep ["invalid type in callClosure:", pPrint t]

bitcast ::
  (MonadReader CodeGenEnv m, MonadIO m) =>
  -- | Type of the value
  Text ->
  -- | Value
  Constant ->
  -- | Type to cast
  Text ->
  m Ident
bitcast from v to = autoRegister do
  putAsmLn $ "bitcast " <> from <> " " <> toAsm v <> " to " <> to

getelementptr :: (MonadReader CodeGenEnv m, MonadIO m) => Text -> Constant -> [Int] -> m Ident
getelementptr typ addr index = autoRegister do
  putAsmLn $ "getelementptr " <> typ <> ", ptr " <> toAsm addr <> ", " <> Text.intercalate "," (map (\i -> "i32 " <> show i) index)

load :: (MonadReader CodeGenEnv m, MonadIO m) => Text -> Constant -> m Ident
load typ addr =
  autoRegister $
    putAsmLn $
      "load " <> typ <> ", ptr " <> toAsm addr

store ::
  (MonadReader CodeGenEnv m, MonadIO m) =>
  -- | Type of the value
  Text ->
  -- | Value
  Constant ->
  -- | Address of the value
  Constant ->
  m ()
store typ value address = do
  putAsmLn $ "store " <> typ <> " " <> toAsm value <> ", ptr " <> toAsm address

mallocBytes ::
  (MonadReader CodeGenEnv m, MonadIO m) =>
  -- | Size
  Constant ->
  m Ident
mallocBytes size = autoRegister do
  putAsmLn $ "call ptr @malgo_malloc(i64 " <> toAsm size <> ")"

mallocType ::
  (MonadReader CodeGenEnv m, MonadIO m) =>
  -- | Type
  Text ->
  m Ident
mallocType typ = autoRegister do
  putAsmLn $ "call ptr @malgo_malloc(i64 " <> sizeOf typ <> ")"
  where
    sizeOf typ = "ptrtoint (ptr getelementptr (" <> typ <> ", ptr null, i32 1) to i64)"

switch :: (MonadReader CodeGenEnv m, MonadIO m) => Constant -> m () -> [(Constant, m ())] -> m ()
switch scrutinee defaultBranch branches = do
  undefined