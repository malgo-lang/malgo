module Koriel.Core.CodeGen.PrintLLVM (codeGen) where

import Control.Lens (Field1, view, _1)
import Data.HashMap.Strict qualified as HashMap
import Data.String.Conversions (convertString)
import Data.Text qualified as Text
import Data.Tuple.Extra (uncurry3)
import Koriel.Core.Syntax hiding (variable)
import Koriel.Core.Type
import Koriel.Id (Id, ModuleName, idToText)
import Koriel.Prelude
import Koriel.Pretty
import LLVM.AST.Constant qualified as C

type KName = Id Type

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
    runReaderT ?? CodeGenEnv {..} $ do
      putAsmLn $ "source_filename = \"" <> toText srcPath <> "\""
      extern "GC_init" [] VoidT

      for_ extFuns \case
        (name, ps :-> r) -> extern name ps r
        _ -> error "invalid type"

      traverse_ (uncurry3 variable) topVars

      define "main" Int32T do
        putAsmLn "\tret i32 0"
  where
    buildInternTable s = HashMap.insert (view _1 s) Global

variable :: KName -> Type -> Expr KName -> ReaderT CodeGenEnv IO ()
variable name _ expr = do
  name <- lookupName name
  putAsm $ name <> " = "
  putGlobalConstant expr
  putAsmLn ""

putGlobalConstant :: Expr KName -> ReaderT CodeGenEnv IO ()
putGlobalConstant (Atom atom) = do
  putAsm "global "
  putAtom atom
putGlobalConstant e = errorDoc $ sep ["not constant:", pPrint e]

putAtom :: Atom KName -> ReaderT CodeGenEnv IO ()
putAtom (Var _) = error "not implemented"
putAtom (Unboxed (Int32 i)) = putAsm $ "i32 " <> show i

data NameSpace = Global | Local

instance Pretty NameSpace where
  pPrint Global = "global"
  pPrint Local = "local"

data CodeGenEnv = CodeGenEnv
  { moduleName :: ModuleName,
    interned :: HashMap KName NameSpace,
    output :: Handle
  }

-- * Assembly builder

putAsmLn :: (MonadReader CodeGenEnv m, MonadIO m) => Text -> m ()
putAsmLn asm = do
  CodeGenEnv {output} <- ask
  liftIO $ hPutTextLn output asm

putAsm :: (MonadReader CodeGenEnv m, MonadIO m) => Text -> m ()
putAsm asm = do
  CodeGenEnv {output} <- ask
  liftIO $ hPutText output asm

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
      pure (\env -> env {interned = HashMap.insert name given interned}, sigil given <> idToText name)

sigil :: NameSpace -> Text
sigil Global = "@"
sigil Local = "%"

lookupName name = do
  CodeGenEnv {interned} <- ask
  case HashMap.lookup name interned of
    Just Global -> pure $ "@" <> idToText name
    Just Local -> pure $ "%" <> idToText name
    Nothing -> errorDoc $ "unbound variable: " <> pPrint name

extern :: (MonadReader CodeGenEnv m, MonadIO m) => Text -> [Type] -> Type -> m ()
extern name params result = do
  putAsmLn $ "declare " <> toAsm result <> " @" <> name <> "(" <> Text.intercalate ", " (map toAsm params) <> ")"

define :: (MonadReader CodeGenEnv m, MonadIO m) => Text -> Type -> m () -> m ()
define name retType body = do
  putAsmLn $ "define " <> toAsm retType <> " @" <> name <> "() {"
  void body
  putAsmLn "}"
