package rename

import (
	"fmt"

	"github.com/takoeight0821/malgo/internal/ast"
)

type AlreadyBoundError struct {
	input string
	pos   int
	ident ast.Ident
}

func (e AlreadyBoundError) Input() string {
	return e.input
}

func (e AlreadyBoundError) Pos() int {
	return e.pos
}

func (e AlreadyBoundError) Error() string {
	return ast.Line(e) + fmt.Sprintf("%s is already bound", e.ident.Name())
}

type UnbondVariableError struct {
	input    string
	variable ast.Variable
}

func (e UnbondVariableError) Input() string {
	return e.input
}

func (e UnbondVariableError) Pos() int {
	return e.variable.Pos()
}

func (e UnbondVariableError) Error() string {
	return ast.Line(e) + fmt.Sprintf("unbound variable: %v", e.variable.Ident.Name())
}
