// Flatten nested (co)patterns into a flatten sequence of (co)patterns.
package flatten

import (
	"log"
	"slices"

	"github.com/takoeight0821/malgo/internal/ast"
)

/*
Examples:

	// Translate copatterns into simple records.
	FlatCopattern({.head (# x xs) -> x, .tail (# x xs) -> xs})
		= { # x xs -> { .head # -> x, .tail # -> xs} }
	FlatCopattern({# x -> x})
		= {# x -> x}
	// Don't translate patterns.
	FlatCopattern({# (.Cons x Nil) -> x}) = {# (.Cons x Nil) -> x}
	// Translate nested copatterns.
	FlatCopattern({.head (.tail (# x xs)) -> x})
		= { # x xs -> { .tail # -> { .head # -> x } } }
*/
func FlatCopattern(input string, expr ast.Expr) ast.Expr {
	return newFlatter(input).flattenExpr(expr)
}

type flatter struct {
	input string
}

func newFlatter(input string) flatter {
	return flatter{input: input}
}

func (f flatter) flattenExpr(expr ast.Expr) ast.Expr {
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
		panic(ast.NewNotExprError(f.input, expr))
	}
}

func (f flatter) flattenApply(expr ast.Apply) ast.Apply {
	fun := f.flattenExpr(expr.Func)
	args := []ast.Node{}
	for _, arg := range expr.Args {
		args = append(args, f.flattenExpr(arg))
	}
	return ast.NewApply(fun, args)
}

func (f flatter) flattenCodata(expr ast.Codata) ast.Expr {
	clauses := []ast.Clause{}
	for _, clause := range expr.Clauses {
		clauses = append(clauses, f.flattenClause(clause))
	}
	return ast.NewCodata(clauses, expr.Pos())
}

func (f flatter) flattenClause(clause ast.Clause) ast.Clause {
	patternSeq := f.toSeq(clause.Pattern)
	log.Printf("[debug] patternSeq: %v", patternSeq)
	slices.Reverse(patternSeq)
	body := f.flattenExpr(clause.Body)
	var newClause ast.Clause
	for _, pattern := range patternSeq {
		newClause = ast.NewClause(pattern, body)
		body = ast.NewCodata([]ast.Clause{newClause}, clause.Pos())
	}
	return newClause
}

/*
Examples:

	toSeq(# x y) = [# x y]
	toSeq(.head (# x xs)) = [# x xs, .head #]
	toSeq(.head (.tail #)) = [.tail #, .head #]
	toSeq(# (.Cons x xs)) = [# (.Cons x xs)]
*/
func (f flatter) toSeq(pattern ast.Pattern) []ast.Pattern {
	switch pattern := pattern.(type) {
	case ast.Variable:
		return []ast.Pattern{pattern}
	case ast.Label:
		return []ast.Pattern{pattern}
	case ast.Literal:
		return []ast.Pattern{pattern}
	case ast.Apply:
		return f.toSeqApply(pattern)
	default:
		panic(ast.NewNotPatternError(f.input, pattern))
	}
}

func (f flatter) toSeqApply(pattern ast.Apply) []ast.Pattern {
	switch pattern.Func.(type) {
	case ast.Label:
		newPattern := ast.NewApply(pattern.Func, []ast.Node{ast.NewThis(pattern.Pos())})
		if len(pattern.Args) != 1 {
			// The arity of the label must be 1.
			panic(ast.NewNotPatternError(f.input, pattern))
		}
		rest := f.toSeq(pattern.Args[0].(ast.Pattern))
		return append(rest, newPattern)
	}
	return []ast.Pattern{pattern}
}
