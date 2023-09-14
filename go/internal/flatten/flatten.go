// Flatten nested (co)patterns into a flatten sequence of (co)patterns.
package flatten

import (
	"github.com/takoeight0821/malgo/internal/ast"
	"github.com/takoeight0821/malgo/internal/rename"
)

func Flatten(input string, expr ast.Expr) ast.Expr {
	return newFlatter(input).flattenExpr(expr)
}

type Flatter struct {
	input string
}

func newFlatter(input string) Flatter {
	return Flatter{input: input}
}

func (f Flatter) isFlatten(pattern ast.Pattern) bool {
	switch pattern.(type) {
	case ast.This:
		return true
	case ast.Variable:
		return true
	case ast.Literal:
		return true
	case ast.Apply:
		return isSimpleExpr(pattern)
	default:
		panic(rename.NewNotPatternError(f.input, pattern))
	}
}

func isSimpleExpr(expr ast.Expr) bool {
	switch expr.(type) {
	case ast.This:
		return true
	case ast.Variable:
		return true
	case ast.Literal:
		return true
	default:
		return false
	}
}

func (f Flatter) flattenExpr(expr ast.Expr) ast.Expr {
	switch expr := expr.(type) {
	case ast.Variable:
		return expr
	case ast.Literal:
		return expr
	case ast.Apply:
		return f.flattenApply(expr)
	case ast.Codata:
		return f.flattenCodata(expr)
	default:
		panic(rename.NewNotExprError(f.input, expr))
	}
}

func (f Flatter) flattenApply(expr ast.Apply) ast.Apply {
	fun := f.flattenExpr(expr.Func)
	args := []ast.Node{}
	for _, arg := range expr.Args {
		args = append(args, f.flattenExpr(arg))
	}
	return ast.NewApply(fun, args)
}

func (f Flatter) flattenCodata(expr ast.Codata) ast.Codata {
	panic("unimplemented")
}
