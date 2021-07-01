#!/usr/bin/env stack
-- stack --resolver lts-18.0 script --package pretty

{-# LANGUAGE OverloadedStrings #-}

import Text.PrettyPrint
import Prelude hiding ((<>))

magmaType = ["int32_t", "int64_t", "float", "double"]

compareType = ["int32_t", "int64_t", "float", "double"]

generateBinOp =
  putStrLn $
    render $
      "#include <gc.h>"
        $$ "#include <inttypes.h>"
        $$ "#include <stdio.h>"
        $$ "#include <stdlib.h>"
        $$ "#include <string.h>"
        $$ "// Arithmetic operators"
        $$ sep (map (genMagma "malgo_add" "+") magmaType)
        $$ sep (map (genMagma "malgo_sub" "-") magmaType)
        $$ sep (map (genMagma "malgo_mul" "*") magmaType)
        $$ sep (map (genMagma "malgo_div" "/") magmaType)
        $$ "// Comparison operators"
        $$ sep (map (genCompare "malgo_eq" "==") compareType)
        $$ sep (map (genCompare "malgo_ne" "!=") compareType)
        $$ sep (map (genCompare "malgo_lt" "<") compareType)
        $$ sep (map (genCompare "malgo_gt" ">") compareType)
        $$ sep (map (genCompare "malgo_le" "<=") compareType)
        $$ sep (map (genCompare "malgo_ge" ">=") compareType)

genMagma opName operator typeName =
  typeName <+> opName <> "_" <> typeName <> parens (typeName <+> "x" <> "," <+> typeName <+> "y")
    <+> braces ("return" <+> "x" <+> operator <+> "y" <> ";")

genCompare opName operator typeName =
  "int32_t" <+> opName <> "_" <> typeName <> parens (typeName <+> "x" <> "," <+> typeName <+> "y")
    <+> braces ("return" <+> "x" <+> operator <+> "y" <> ";")

main = generateBinOp
