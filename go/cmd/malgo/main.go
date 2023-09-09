package main

import (
	"fmt"

	"github.com/takoeight0821/malgo/internal/ast"
	"github.com/takoeight0821/malgo/internal/parser"
	"github.com/takoeight0821/malgo/internal/rename"
)

func main() {
	parser := parser.NewParser("{head (# x xs) -> x, tail (# x xs) -> xs}")
	expr := parser.Parse()
	fmt.Printf("%v\n", expr)
	fvs := ast.FreeVariable(expr)
	for _, fv := range fvs.ToSlice() {
		fmt.Printf("free variable: %v\n", fv)
	}
	if len(fvs.ToSlice()) != 0 {
		panic("free variables exist")
	}
	renamed := rename.Rename(expr)
	fmt.Printf("%v\n", renamed)
}
