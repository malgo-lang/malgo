{-# OPTIONS_GHC -F -pgmF hspec-discover #-}

import           Test.Hspec
import Prelude

spec = return ()

main :: IO ()
main = hspec spec
