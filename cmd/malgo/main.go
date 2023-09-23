package main

import (
	"fmt"

	"github.com/malgo-lang/malgo/internal/ast"
	"github.com/malgo-lang/malgo/internal/parser"
	"github.com/malgo-lang/malgo/internal/rename"
)

func main() {
	input := "{.head (# x xs) -> x, .tail (# x xs) -> xs}"
	parser := parser.NewParser(input)
	expr := parser.Parse()
	fmt.Printf("%v\n", expr)
	info := ast.NewInfo(input)
	fvs := ast.FreeVariable(*info, expr)
	for _, fv := range fvs.ToSlice() {
		fmt.Printf("free variable: %v\n", fv)
	}
	if len(fvs.ToSlice()) != 0 {
		panic("free variables exist")
	}
	renamed := rename.Rename(info, expr)
	fmt.Printf("%v\n", renamed)
}
