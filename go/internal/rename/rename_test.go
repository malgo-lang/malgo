package rename_test

import (
	"testing"

	"github.com/takoeight0821/malgo/internal/parser"
	"github.com/takoeight0821/malgo/internal/rename"
)

func TestRename(t *testing.T) {
	t.Parallel()

	source := "{head (# x xs) -> x, tail (# x xs) -> {# a -> x} xs}"
	ast := parser.NewParser(source).Parse()
	renamed := rename.Rename(source, ast)
	expect := "{(head_0 (# x_0 xs_0)) -> x_0, (tail_0 (# x_1 xs_1)) -> ({(# a_0) -> x_1} xs_1)}"
	if !(renamed.String() == expect) {
		t.Errorf(
			"rename.Rename(%v)\n    = %v,\n want %v",
			source,
			renamed,
			expect)
	}
}
