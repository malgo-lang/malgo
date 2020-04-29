import Distribution.Simple
import Distribution.Simple.Setup (BuildFlags)
import Distribution.Types.HookedBuildInfo
  ( HookedBuildInfo,
    emptyHookedBuildInfo,
  )
import Hpack

main = do
  defaultMainWithHooks $
    simpleUserHooks
      { preBuild = \_ _ -> hpack Verbose defaultOptions >> return emptyHookedBuildInfo
      }
