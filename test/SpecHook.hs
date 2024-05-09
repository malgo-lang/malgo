module SpecHook (hook) where

import Malgo.Prelude
import Malgo.TestUtils
import System.Directory (createDirectory, doesDirectoryExist, getCurrentDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import Test.Hspec

hook :: SpecWith () -> SpecWith ()
hook = parallel . beforeAll setup
  where
    setup = do
      pwd <- getCurrentDirectory
      isExist <- doesDirectoryExist $ pwd </> ".malgo-work"
      when isExist do
        removeDirectoryRecursive $ pwd </> ".malgo-work"
      createDirectory $ pwd </> ".malgo-work"
      createDirectory $ pwd </> ".malgo-work" </> "libs"
      setupRuntime
      setupBuiltin
      setupPrelude