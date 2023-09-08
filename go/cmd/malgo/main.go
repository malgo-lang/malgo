package main

import (
	"fmt"

	"github.com/takoeight0821/malgo/internal/ast"
	"github.com/takoeight0821/malgo/internal/parser"
)

func main() {
	parser := parser.NewParser("{# x y -> print x}")
	expr := parser.Parse()
	fmt.Printf("%v\n", expr)
	fvs := ast.FreeVariable(expr)
	fmt.Printf("%v\n", fvs)
}
