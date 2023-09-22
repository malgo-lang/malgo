package rename_test

import (
	"testing"

	"github.com/malgo-lang/malgo/internal/ast"
	"github.com/malgo-lang/malgo/internal/parser"
	"github.com/malgo-lang/malgo/internal/rename"
)

func TestRename(t *testing.T) {
	t.Parallel()

	source := "{.head (# x xs) -> x, .tail (# x xs) -> {# a -> x} xs}"
	parsed := parser.NewParser(source).Parse()
	info := ast.NewInfo(source)
	renamed := rename.Rename(info, parsed)
	expect := "{(.head (# x_0 xs_0)) -> x_0, (.tail (# x_1 xs_1)) -> ({(# a_0) -> x_1} xs_1)}"
	if !(renamed.String() == expect) {
		t.Errorf(
			"rename.Rename(%v)\n    = %v,\n want %v",
			source,
			renamed,
			expect)
	}
}
