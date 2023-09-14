package closure

import "github.com/takoeight0821/malgo/internal/ast"

// Special case of Codata.
// All Lambda has no free variables.
type Lambda struct {
	// Free variables in the Body.
	Parameters []ast.Ident
	Body       ast.Expr
}

// IsExpr implements ast.Expr.
func (Lambda) IsExpr() bool {
	panic("unimplemented")
}

// IsPattern implements ast.Expr.
func (Lambda) IsPattern() bool {
	panic("unimplemented")
}

// Pos implements ast.Expr.
func (Lambda) Pos() int {
	panic("unimplemented")
}

// String implements ast.Expr.
func (Lambda) String() string {
	panic("unimplemented")
}

var _ ast.Expr = Lambda{}
