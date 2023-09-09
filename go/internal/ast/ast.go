package ast

import "fmt"

type Node interface {
	fmt.Stringer
	Pos() int // byte position of start of node
}

type Expr interface {
	Node
}

type Ident interface {
	Name() string
}

type String string

func (s String) Name() string {
	return string(s)
}

type Variable struct {
	Ident       Ident
	variablePos int
}

func NewVariable(name Ident, pos int) Variable {
	return Variable{
		Ident:       name,
		variablePos: pos,
	}
}

func (v Variable) Pos() int {
	return v.variablePos
}

func (v Variable) Arity() int {
	return 0
}

func (v Variable) String() string {
	return v.Ident.Name()
}

var (
	_ Expr    = Variable{}
	_ Pattern = Variable{}
)

type Literal struct {
	Value      int
	literalPos int
}

func NewLiteral(value int, pos int) Literal {
	return Literal{
		Value:      value,
		literalPos: pos,
	}
}

func (l Literal) Pos() int {
	return l.literalPos
}

func (l Literal) Arity() int {
	return 0
}

func (l Literal) String() string {
	return fmt.Sprintf("%d", l.Value)
}

var (
	_ Expr    = Literal{}
	_ Pattern = Literal{}
)

type Apply struct {
	Func Node
	Args []Node
}

func NewApply(f Node, args []Node) Apply {
	return Apply{
		Func: f,
		Args: args,
	}
}

func (a Apply) Pos() int {
	return a.Func.Pos()
}

func (a Apply) Arity() int {
	// If a.Func is a This, then return the length of a.Args.
	// otherwise, return the maximum of a.Func.Arity() and the length of a.Args.
	if _, ok := a.Func.(This); ok {
		return len(a.Args)
	}
	if f, ok := a.Func.(Pattern); ok {
		arity := f.Arity()
		for _, arg := range a.Args {
			if arg, ok := arg.(Pattern); ok {
				arity = max(arity, arg.Arity())
			}
		}
	}
	panic(a.String() + " is not a pattern")
}

func (a Apply) String() string {
	str := "(" + a.Func.String()
	for _, arg := range a.Args {
		str += " " + arg.String()
	}
	str += ")"
	return str
}

var (
	_ Expr    = Apply{}
	_ Pattern = Apply{}
)

type Codata struct {
	Clauses   []Clause
	clausePos int
}

func NewCodata(clauses []Clause, pos int) Codata {
	return Codata{
		Clauses:   clauses,
		clausePos: pos,
	}
}

func (c Codata) Pos() int {
	return c.clausePos
}

func (c Codata) String() string {
	str := "{"
	for i, clause := range c.Clauses {
		if i > 0 {
			str += ", "
		}
		str += clause.String()
	}
	str += "}"
	return str
}

var _ Expr = Codata{}

type Clause struct {
	Pattern Pattern
	Body    Expr
}

func NewClause(pattern Pattern, body Expr) Clause {
	return Clause{
		Pattern: pattern,
		Body:    body,
	}
}

func (c Clause) Pos() int {
	return c.Pattern.Pos()
}

func (c Clause) String() string {
	return c.Pattern.String() + " -> " + c.Body.String()
}

var _ Node = Clause{}

type Pattern interface {
	Node
	Arity() int
}

type This struct {
	thisPos int
}

func NewThis(pos int) This {
	return This{
		thisPos: pos,
	}
}

func (p This) Pos() int {
	return p.thisPos
}

func (p This) Arity() int {
	return 0
}

func (p This) String() string {
	return "#"
}

var _ Pattern = This{}
