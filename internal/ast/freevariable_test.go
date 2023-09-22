package ast_test

import (
	"testing"

	mapset "github.com/deckarep/golang-set/v2"
	"github.com/malgo-lang/malgo/internal/ast"
	"github.com/malgo-lang/malgo/internal/parser"
)

func TestFreeVariable(t *testing.T) {
	t.Parallel()

	source := "{.head (# x xs) -> x, .tail (# x xs) -> {# a -> x} xs}"
	parsed := parser.NewParser(source).Parse()
	info := ast.NewInfo(source)
	freevars := ast.FreeVariable(*info, parsed)
	expect := mapset.NewSet[ast.Ident]()
	if !freevars.Equal(expect) {
		t.Errorf(
			"ast.FreeVariable(%v)\n    = %v,\n want %v",
			source,
			freevars,
			expect)
	}
}
