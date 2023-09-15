// Flatten nested (co)patterns into a flatten sequence of (co)patterns.
package flatten

import (
	"log"

	"github.com/takoeight0821/malgo/internal/ast"
	"github.com/takoeight0821/malgo/internal/rename"
)

/*
Examples:

	// Translate copatterns into simple records.
	FlatCopattern({.head (# x xs) -> x, .tail (# x xs) -> xs})
		= { # x xs -> { .head : x, .tail : xs} }
	FlatCopattern({# x -> x})
		= {# x -> x}
	// Don't translate patterns.
	FlatCopattern({# (.Cons x Nil) -> x}) = {# (.Cons x Nil) -> x}
	// Translate nested copatterns.
	FlatCopattern({.head (.tail (# x xs)) -> x})
		= { # x xs -> { .tail : { .head : x } } }
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
		panic(rename.NewNotExprError(f.input, expr))
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
	panic("TODO")
}

func (f flatter) flattenClause(clause ast.Clause) ast.Expr {
	patternSeq := f.toSeq(clause.Pattern)
	log.Printf("[debug] patternSeq: %v", patternSeq)
	panic("TODO")
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
		panic(rename.NewNotPatternError(f.input, pattern))
	}
}

func (f flatter) toSeqApply(pattern ast.Apply) []ast.Pattern {
	switch pattern.Func.(type) {
	case ast.Label:
		newPattern := ast.NewApply(pattern, []ast.Node{ast.NewThis(pattern.Pos())})
		if len(pattern.Args) != 1 {
			// The arity of the label must be 1.
			panic(rename.NewNotPatternError(f.input, pattern))
		}
		rest := f.toSeq(pattern.Args[0].(ast.Pattern))
		return append(rest, newPattern)
	}
	return []ast.Pattern{pattern}
}

// Special case of Codata.
type Object struct {
	pos    int
	Fields map[ast.Ident]ast.Expr
}

func (Object) IsExpr() bool {
	return true
}

func (Object) IsPattern() bool {
	return false
}

func (o Object) Pos() int {
	return o.pos
}

func (o Object) String() string {
	str := "{ "
	for label, value := range o.Fields {
		str += label.Name() + ": " + value.String() + ", "
	}
	str += "}"
	return str
}

var _ ast.Expr = Object{}

/*
// Special case of Codata.
type Lambda struct {
	// Free variables in the Body.
	Parameters []ast.Ident
	Body       ast.Expr
}

func (Lambda) IsExpr() bool {
	return true
}

func (Lambda) IsPattern() bool {
	return false
}

func (l Lambda) Pos() int {
	return l.Body.Pos()
}

func (l Lambda) String() string {
	str := "{"
	for i, param := range l.Parameters {
		if i != 0 {
			str += " "
		}
		str += param.Name()
	}
	str += " -> " + l.Body.String() + "}"
	return str
}

var _ ast.Expr = Lambda{}
type Switch struct {
	Target ast.Expr
	Cases  map[ast.Ident]ast.Expr
}

func (Switch) IsExpr() bool {
	return true
}

func (Switch) IsPattern() bool {
	return false
}

func (s Switch) Pos() int {
	return s.Target.Pos()
}

func (s Switch) String() string {
	str := "switch " + s.Target.String() + " {"
	for label, value := range s.Cases {
		str += label.Name() + ": " + value.String() + ", "
	}
	str += "}"
	return str
}

var _ ast.Expr = Switch{}

type Select struct {
	Target ast.Expr
	Index  int
	Bind   ast.Ident
	Body   ast.Expr
}

func (Select) IsExpr() bool {
	return true
}

func (Select) IsPattern() bool {
	return false
}

func (s Select) Pos() int {
	return s.Target.Pos()
}

func (s Select) String() string {
	return "select " + s.Bind.Name() + " = " + s.Target.String() + "[" + fmt.Sprint(s.Index) + "] in " + s.Body.String()
}

var _ ast.Expr = Select{}

type Error struct {
	pos int
}

func (Error) IsExpr() bool {
	return true
}

func (Error) IsPattern() bool {
	return false
}

func (e Error) Pos() int {
	return e.pos
}

func (e Error) String() string {
	return "ERROR"
}

var _ ast.Expr = Error{}

type TryOr struct {
	Left  ast.Expr
	Right ast.Expr
}

func (TryOr) IsExpr() bool {
	return true
}

func (TryOr) IsPattern() bool {
	return false
}

func (t TryOr) Pos() int {
	return t.Left.Pos()
}

func (t TryOr) String() string {
	return t.Left.String() + " [] " + t.Right.String()
}

var _ ast.Expr = TryOr{}
*/
