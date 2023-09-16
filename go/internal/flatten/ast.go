package flatten

import (
	"fmt"

	"github.com/takoeight0821/malgo/internal/alpha"
	"github.com/takoeight0821/malgo/internal/ast"
)

// Special case of Codata.
type Object struct {
	pos    int
	Fields map[ast.Ident]ast.Expr
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

// Special case of Codata.
type LambdaCase struct {
	Parameters []ast.Ident
	Cases      []Case
}

func (l LambdaCase) Pos() int {
	return l.Cases[0].Pattern.Pos()
}

func (l LambdaCase) String() string {
	str := "(\\"
	for _, param := range l.Parameters {
		str += param.Name() + " "
	}
	str += "-> "
	for _, c := range l.Cases {
		str += c.Pattern.String() + " -> " + c.Body.String() + ", "
	}
	str += ")"
	return str
}

var _ ast.Expr = LambdaCase{}

type Case struct {
	Pattern ast.Pattern
	Body    ast.Expr
}

// Special case of Codata.
type Lambda struct {
	// Free variables in the Body.
	Parameters []ast.Ident
	Body       ast.Expr
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

func (s Select) Pos() int {
	return s.Target.Pos()
}

func (s Select) String() string {
	return "select " + s.Bind.Name() + " = " + s.Target.String() + "[" + fmt.Sprint(s.Index) + "] in " + s.Body.String()
}

var _ ast.Expr = Select{}

// {coalt^n} p^m
// p is a pattern, not a copattern.
func staticApply(lambda LambdaCase, args []ast.Node) []Case {
	subst := make(map[ast.Ident]ast.Node)
	for i, param := range lambda.Parameters {
		subst[param] = args[i]
	}

	cases := make([]Case, 0, len(lambda.Cases))
	for _, c := range lambda.Cases {
		cases = append(cases, Case{Pattern: alpha.Convert(c.Pattern, subst).(ast.Pattern), Body: alpha.Convert(c.Body, subst)})
	}
	return cases
}

// H_i {coalt^n}
func staticAccess(label ast.Ident, object Object) ast.Expr {
	return object.Fields[label]
}
