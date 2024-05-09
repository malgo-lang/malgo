module SpecHook (hook) where

import Malgo.Prelude
import Malgo.TestUtils
import System.Directory (createDirectory, getCurrentDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import Test.Hspec

hook :: SpecWith () -> SpecWith ()
hook = parallel . beforeAll setup
  where
    setup = do
      pwd <- getCurrentDirectory
      removeDirectoryRecursive $ pwd </> ".malgo-work"
      createDirectory $ pwd </> ".malgo-work"
      createDirectory $ pwd </> ".malgo-work" </> "libs"
      setupRuntime
      setupBuiltin
      setupPrelude