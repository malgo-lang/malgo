{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}
module Language.Malgo.Utils where

import           Data.Array.IArray
import           Language.Malgo.FrontEnd.Loc
import           Universum

-- file operations
newtype File = File (Array Line Text)
  deriving (Show, Eq)

newFile :: Text -> File
newFile file = File $ listArray (1, length xs) xs
  where xs = lines file

viewSpan :: SrcInfo s => s -> File -> Text
viewSpan s (File f)
  | sline == eline =
    let line = f ! sline
    in toText $ drop (scolumn - 1) $ take (ecolumn - 1) $ toString line
  | otherwise =
    let firstline = toText $ drop (scolumn - 1) $ toString $ f ! sline
        endline = toText $ take (ecolumn - 1) $ toString $ f ! eline
        middle = map (f !) [sline + 1 .. eline - 1]
    in unlines $ [firstline] <> middle <> [endline]
  where
    sline = startLine s
    scolumn = startColumn s
    eline = endLine s
    ecolumn = endColumn s
