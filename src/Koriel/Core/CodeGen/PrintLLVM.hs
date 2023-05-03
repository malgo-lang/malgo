module Koriel.Core.CodeGen.PrintLLVM (codeGen) where

import Koriel.Core.Syntax (Program (..))
import Koriel.Core.Type
import Koriel.Id (ModuleName)
import Koriel.Prelude

-- | Generate and Write LLVM IR to file from a Koriel module.
codeGen ::
  -- | Source path
  FilePath ->
  -- | Destination path
  FilePath ->
  ModuleName ->
  Program a ->
  IO ()
codeGen srcPath dstPath moduleName Program {} = do
  withFile dstPath WriteMode \output -> do
    runReaderT ?? CodeGenEnv {..} $ do
      putAsm $ "source_filename = \"" <> toText srcPath <> "\""
      define "main" Int32T do
        putAsm "\tret i32 0"

data CodeGenEnv = CodeGenEnv
  { moduleName :: ModuleName,
    output :: Handle
  }

putAsm :: (MonadReader CodeGenEnv m, MonadIO m) => Text -> m ()
putAsm asm = do
  CodeGenEnv {output} <- ask
  liftIO $ hPutTextLn output asm

class ToAsm a where
  toAsm :: a -> Text

instance ToAsm Type where
  toAsm :: Type -> Text
  toAsm (_ :-> _) = "ptr"
  toAsm Int32T = "i32"
  toAsm Int64T = "i64"
  toAsm FloatT = "f32"
  toAsm DoubleT = "f64"
  toAsm CharT = "i8"
  toAsm StringT = "ptr"
  toAsm BoolT = "i1"
  toAsm (SumT _) = "ptr"
  toAsm (PtrT _) = "ptr"
  toAsm (RecordT _) = "ptr"
  toAsm AnyT = "ptr"
  toAsm VoidT = "void"

define :: (MonadReader CodeGenEnv m, MonadIO m) => Text -> Type -> m () -> m ()
define name retType body = do
  putAsm $ "define " <> toAsm retType <> " @" <> name <> "() {"
  void body
  putAsm "}"
