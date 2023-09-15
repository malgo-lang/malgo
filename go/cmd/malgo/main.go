package main

import (
	"fmt"

	"github.com/takoeight0821/malgo/internal/ast"
	"github.com/takoeight0821/malgo/internal/flatten"
	"github.com/takoeight0821/malgo/internal/parser"
	"github.com/takoeight0821/malgo/internal/rename"
)

func main() {
	input := "{.head (# x xs) -> x, .tail (# x xs) -> xs}"
	parser := parser.NewParser(input)
	expr := parser.Parse()
	fmt.Printf("%v\n", expr)
	// TODO: more user-friendly error message
	fvs := ast.FreeVariable(expr)
	for _, fv := range fvs.ToSlice() {
		fmt.Printf("free variable: %v\n", fv)
	}
	if len(fvs.ToSlice()) != 0 {
		panic("free variables exist")
	}
	renamed := rename.Rename(input, expr)
	fmt.Printf("%v\n", renamed)
	copatternFlattened := flatten.FlatCopattern(input, renamed)
	fmt.Printf("%v\n", copatternFlattened)
}
