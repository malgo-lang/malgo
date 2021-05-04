{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Runner where

import qualified Data.Text.IO as T
import Development.Shake
import Development.Shake.FilePath
import Koriel.Id (_ModuleName)
import qualified Language.Malgo.Driver as Driver
import Language.Malgo.Parser (parseMalgo)
import Language.Malgo.Prelude hiding ((<.>))
import Language.Malgo.Syntax
import Language.Malgo.Syntax.Extension
import Text.Megaparsec (errorBundlePretty)

runShakeRules :: FilePath -> Opt -> Rules () -> IO ()
runShakeRules workDir opt =
  shake
    shakeOptions
      { shakeFiles = workDir,
        shakeProgress = progressDisplay 5 putStrLn,
        shakeRebuild =
          if forceRebuild opt
            then shakeRebuild shakeOptions <> [(RebuildNow, "**")]
            else shakeRebuild shakeOptions <> [(RebuildNormal, "**")]
      }

mlgToLLShake :: Opt -> Rules ()
mlgToLLShake opt = do
  let dstFile = normaliseEx (dstName opt)
  want [dstFile]
  mlgToLLShake' opt

mlgToLLShake' :: Opt -> Rules ()
mlgToLLShake' opt = do
  let srcFile = normaliseEx (srcName opt)
  let dstFile = normaliseEx (dstName opt)
  parsedAst <- liftIO $ do
    src <- T.readFile srcFile
    case parseMalgo srcFile src of
      Right x -> pure x
      Left err -> error $ errorBundlePretty err
  sopt <- getShakeOptionsRules
  [normaliseEx dstFile, normaliseEx (dstFile -<.> "mlgi")] |%> \out -> do
    depends <- resolveDepends parsedAst
    need (srcFile : depends)
    traced "compile" $
      Driver.compileFromAST
        parsedAst
        opt
          { dstName = out -<.> "ll",
            modulePaths = [shakeFiles sopt </> "build"]
          }

buildUnderModulePaths :: Opt -> Rules ()
buildUnderModulePaths opt = do
  sopt <- getShakeOptionsRules
  mlgFiles <-
    liftIO $
      concat
        <$> traverse (\path -> map (path </>) <$> getDirectoryFilesIO path ["*.mlg"]) (modulePaths opt)
  for_ mlgFiles \mlgFile -> do
    mlgToLLShake' ((defaultOpt mlgFile) {dstName = shakeFiles sopt </> "build" </> takeFileName mlgFile -<.> "ll"})

resolveDepends :: Monad m => Module (Malgo 'Parse) -> m [FilePath]
resolveDepends Module {_moduleDefinition = ds} = do
  let needModuleNames = mapMaybe (preview $ _Import . _2 . _ModuleName) ds
  pure $ map (\needModuleName -> normaliseEx $ ".malgo-work/build" </> needModuleName <.> "mlgi") needModuleNames
