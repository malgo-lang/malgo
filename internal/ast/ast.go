package ast

import "fmt"

// Node is an interface for all nodes in the AST.
type Node interface {
	fmt.Stringer
	Pos() int // byte position of start of node
}

// Expr is an interface for all expressions in the AST.
// Expr is defined as same interface as Node, but it is semantically different.
//
// For example, This (#) is a pattern but it is not an expression. Codata is a expression but it is not a pattern.
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

type Label struct {
	Ident    Ident
	labelPos int
}

func NewLabel(name Ident, pos int) Label {
	return Label{
		Ident:    name,
		labelPos: pos,
	}
}

func (l Label) Pos() int {
	return l.labelPos
}

func (l Label) Arity() int {
	return 0
}

func (l Label) String() string {
	return "." + l.Ident.Name()
}

var (
	_ Expr    = Label{}
	_ Pattern = Label{}
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

// Used until flattening.
type Codata struct {
	Clauses   []Clause
	codataPos int
}

func NewCodata(clauses []Clause, pos int) Codata {
	return Codata{
		Clauses:   clauses,
		codataPos: pos,
	}
}

func (c Codata) Pos() int {
	return c.codataPos
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

// Used until flattening.
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

// Used until flattening.
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

// Used after flattening.

// Special case of Codata.
type Object struct {
	pos    int
	Fields map[Ident]Expr
}

func NewObject(fields map[Ident]Expr, pos int) Object {
	return Object{
		pos:    pos,
		Fields: fields,
	}
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

var _ Expr = Object{}

// Special case of Codata.
type LambdaCase struct {
	Parameters []Ident
	Cases      []Clause
}

func NewLambdaCase(parameters []Ident, cases []Clause) LambdaCase {
	return LambdaCase{
		Parameters: parameters,
		Cases:      cases,
	}
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

var _ Expr = LambdaCase{}

// Use after closure conversion.

// Special case of Codata.
type Lambda struct {
	// Free variables in the Body.
	Parameters []Ident
	Body       Expr
}

func NewLambda(parameters []Ident, body Expr) Lambda {
	return Lambda{
		Parameters: parameters,
		Body:       body,
	}
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

var _ Expr = Lambda{}

type Switch struct {
	Target Expr
	Cases  map[Ident]Expr
}

func NewSwitch(target Expr, cases map[Ident]Expr) Switch {
	return Switch{
		Target: target,
		Cases:  cases,
	}
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

var _ Expr = Switch{}

type Select struct {
	Target Expr
	Index  int
	Bind   Ident
	Body   Expr
}

func NewSelect(target Expr, index int, bind Ident, body Expr) Select {
	return Select{
		Target: target,
		Index:  index,
		Bind:   bind,
		Body:   body,
	}
}

func (s Select) Pos() int {
	return s.Target.Pos()
}

func (s Select) String() string {
	return "select " + s.Bind.Name() + " = " + s.Target.String() + "[" + fmt.Sprint(s.Index) + "] in " + s.Body.String()
}

var _ Expr = Select{}
