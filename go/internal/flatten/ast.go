package flatten

import (
	"fmt"

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
