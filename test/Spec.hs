{-# OPTIONS_GHC -F -pgmF hspec-discover #-}

import           Prelude
import           Test.Hspec

spec = return ()

main :: IO ()
main = hspec spec
