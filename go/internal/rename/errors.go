package rename

import (
	"fmt"

	"github.com/takoeight0821/malgo/internal/ast"
	"github.com/takoeight0821/malgo/internal/parser"
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
	return parser.Line(e) + fmt.Sprintf("%s is already bound", e.ident.Name())
}

type NotExprError struct {
	input string
	expr  ast.Node
}

func (e NotExprError) Input() string {
	return e.input
}

func (e NotExprError) Pos() int {
	return e.expr.Pos()
}

func (e NotExprError) Error() string {
	return parser.Line(e) + fmt.Sprintf("%v is not an expression", e.expr)
}

type NotPatternError struct {
	input   string
	pattern ast.Node
}

func (e NotPatternError) Input() string {
	return e.input
}

func (e NotPatternError) Pos() int {
	return e.pattern.Pos()
}

func (e NotPatternError) Error() string {
	return parser.Line(e) + fmt.Sprintf("%v is not a pattern", e.pattern)
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
	return parser.Line(e) + fmt.Sprintf("unbound variable: %v", e.variable.Ident.Name())
}
