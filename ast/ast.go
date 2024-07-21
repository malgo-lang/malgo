package ast

import (
	"fmt"
	"log"

	"github.com/takoeight0821/malgo/token"
	"github.com/takoeight0821/malgo/utils"
)

// AST

type Node interface {
	fmt.Stringer
	Base() token.Token
	// Plate applies the given function to each child node.
	// If f returns an error, f also must return the original argument n.
	// It is similar to Visitor pattern.
	// FYI: https://hackage.haskell.org/package/lens-5.2.3/docs/Control-Lens-Plated.html
	Plate(err error, fun func(Node, error) (Node, error)) (Node, error)
}

type Var struct {
	Name token.Token
}

func (v Var) String() string {
	return utils.Parenthesize("var", v.Name).String()
}

func (v *Var) Base() token.Token {
	return v.Name
}

func (v *Var) Plate(err error, _ func(Node, error) (Node, error)) (Node, error) {
	return v, err
}

var _ Node = &Var{}

type Literal struct {
	token.Token
}

func (l Literal) String() string {
	return utils.Parenthesize("literal", l.Token).String()
}

func (l *Literal) Base() token.Token {
	return l.Token
}

func (l *Literal) Plate(err error, _ func(Node, error) (Node, error)) (Node, error) {
	return l, err
}

var _ Node = &Literal{}

type Symbol struct {
	Name token.Token
}

func (s Symbol) String() string {
	return utils.Parenthesize("symbol", s.Name).String()
}

func (s *Symbol) Base() token.Token {
	return s.Name
}

func (s *Symbol) Plate(err error, _ func(Node, error) (Node, error)) (Node, error) {
	return s, err
}

var _ Node = &Symbol{}

type Paren struct {
	Expr Node
}

func (p Paren) String() string {
	return utils.Parenthesize("paren", p.Expr).String()
}

func (p *Paren) Base() token.Token {
	return p.Expr.Base()
}

func (p *Paren) Plate(err error, f func(Node, error) (Node, error)) (Node, error) {
	p.Expr, err = f(p.Expr, err)

	return p, err
}

var _ Node = &Paren{}

type Tuple struct {
	Where token.Token
	Exprs []Node
}

func (t Tuple) String() string {
	return utils.Parenthesize("tuple", utils.Concat[Node](t.Exprs)).String()
}

func (t *Tuple) Base() token.Token {
	return t.Where
}

func (t *Tuple) Plate(err error, f func(Node, error) (Node, error)) (Node, error) {
	for i, expr := range t.Exprs {
		t.Exprs[i], err = f(expr, err)
	}

	return t, err
}

type Access struct {
	Receiver Node
	Name     token.Token
}

func (a Access) String() string {
	return utils.Parenthesize("access", a.Receiver, a.Name).String()
}

func (a *Access) Base() token.Token {
	return a.Name
}

func (a *Access) Plate(err error, f func(Node, error) (Node, error)) (Node, error) {
	a.Receiver, err = f(a.Receiver, err)

	return a, err
}

var _ Node = &Access{}

type Call struct {
	Func Node
	Args []Node
}

func (c Call) String() string {
	return utils.Parenthesize("call", c.Func, utils.Concat(c.Args)).String()
}

func (c *Call) Base() token.Token {
	return c.Func.Base()
}

func (c *Call) Plate(err error, f func(Node, error) (Node, error)) (Node, error) {
	c.Func, err = f(c.Func, err)
	for i, arg := range c.Args {
		c.Args[i], err = f(arg, err)
	}

	return c, err
}

var _ Node = &Call{}

type Prim struct {
	Name token.Token
	Args []Node
}

func (p Prim) String() string {
	return utils.Parenthesize("prim", p.Name, utils.Concat(p.Args)).String()
}

func (p *Prim) Base() token.Token {
	return p.Name
}

func (p *Prim) Plate(err error, f func(Node, error) (Node, error)) (Node, error) {
	for i, arg := range p.Args {
		p.Args[i], err = f(arg, err)
	}

	return p, err
}

var _ Node = &Prim{}

type Binary struct {
	Left  Node
	Op    token.Token
	Right Node
}

func (b Binary) String() string {
	return utils.Parenthesize("binary", b.Left, b.Op, b.Right).String()
}

func (b *Binary) Base() token.Token {
	return b.Op
}

func (b *Binary) Plate(err error, f func(Node, error) (Node, error)) (Node, error) {
	b.Left, err = f(b.Left, err)
	b.Right, err = f(b.Right, err)

	return b, err
}

var _ Node = &Binary{}

type Assert struct {
	Expr Node
	Type Node
}

func (a Assert) String() string {
	return utils.Parenthesize("assert", a.Expr, a.Type).String()
}

func (a *Assert) Base() token.Token {
	return a.Expr.Base()
}

func (a *Assert) Plate(err error, f func(Node, error) (Node, error)) (Node, error) {
	a.Expr, err = f(a.Expr, err)
	a.Type, err = f(a.Type, err)

	return a, err
}

var _ Node = &Assert{}

type Let struct {
	Bind Node
	Body Node
}

func (l Let) String() string {
	return utils.Parenthesize("let", l.Bind, l.Body).String()
}

func (l *Let) Base() token.Token {
	return l.Bind.Base()
}

func (l *Let) Plate(err error, f func(Node, error) (Node, error)) (Node, error) {
	l.Bind, err = f(l.Bind, err)
	l.Body, err = f(l.Body, err)

	return l, err
}

var _ Node = &Let{}

type With struct {
	Binds []Node
	Body  Node
}

func (w With) String() string {
	return utils.Parenthesize("with", utils.Concat(w.Binds), w.Body).String()
}

func (w *With) Base() token.Token {
	if len(w.Binds) == 0 {
		return w.Body.Base()
	}

	return w.Binds[0].Base()
}

func (w *With) Plate(err error, f func(Node, error) (Node, error)) (Node, error) {
	for i, bind := range w.Binds {
		w.Binds[i], err = f(bind, err)
	}
	w.Body, err = f(w.Body, err)

	return w, err
}

var _ Node = &With{}

type Seq struct {
	Exprs []Node
}

func (s Seq) String() string {
	return utils.Parenthesize("seq", utils.Concat(s.Exprs)).String()
}

func (s *Seq) Base() token.Token {
	if len(s.Exprs) == 0 {
		return token.Token{}
	}

	return s.Exprs[0].Base()
}

func (s *Seq) Plate(err error, f func(Node, error) (Node, error)) (Node, error) {
	for i, expr := range s.Exprs {
		s.Exprs[i], err = f(expr, err)
	}

	return s, err
}

var _ Node = &Seq{}

type Codata struct {
	// len(Clauses) > 0
	// for each clause, len(Patterns) == 1
	Clauses []*CodataClause
}

func (c Codata) String() string {
	return utils.Parenthesize("codata", utils.Concat(c.Clauses)).String()
}

func (c *Codata) Base() token.Token {
	if len(c.Clauses) == 0 {
		return token.Token{}
	}

	return c.Clauses[0].Base()
}

func (c *Codata) Plate(err error, f func(Node, error) (Node, error)) (Node, error) {
	for i, clause := range c.Clauses {
		var cl Node
		cl, err = f(clause, err)
		theCl, ok := cl.(*CodataClause)
		if !ok {
			log.Panicf("invalid clause: %v", cl)
		}

		c.Clauses[i] = theCl
	}

	return c, err
}

var _ Node = &Codata{}

type CodataClause struct {
	Pattern Node
	Expr    Node
}

func (c CodataClause) String() string {
	return utils.Parenthesize("clause", c.Pattern, c.Expr).String()
}

func (c *CodataClause) Base() token.Token {
	if c.Pattern != nil {
		return c.Pattern.Base()
	}

	return c.Expr.Base()
}

func (c *CodataClause) Plate(err error, f func(Node, error) (Node, error)) (Node, error) {
	c.Pattern, err = f(c.Pattern, err)
	c.Expr, err = f(c.Expr, err)

	return c, err
}

var _ Node = &CodataClause{}

type Lambda struct {
	Params []token.Token
	Expr   Node
}

func (l Lambda) String() string {
	return utils.Parenthesize("lambda", utils.Parenthesize("", utils.Concat(l.Params)), l.Expr).String()
}

func (l *Lambda) Base() token.Token {
	if len(l.Params) == 0 {
		return l.Expr.Base()
	}

	return l.Params[0]
}

func (l *Lambda) Plate(err error, f func(Node, error) (Node, error)) (Node, error) {
	l.Expr, err = f(l.Expr, err)

	return l, err
}

var _ Node = &Lambda{}

type Case struct {
	Scrutinees []Node
	Clauses    []*CaseClause // len(Clauses) > 0
}

func (c Case) String() string {
	return utils.Parenthesize("case", utils.Parenthesize("", utils.Concat(c.Scrutinees)), utils.Concat(c.Clauses)).String()
}

func (c *Case) Base() token.Token {
	return c.Scrutinees[0].Base()
}

func (c *Case) Plate(err error, fun func(Node, error) (Node, error)) (Node, error) {
	for i, scrutinee := range c.Scrutinees {
		c.Scrutinees[i], err = fun(scrutinee, err)
	}
	for i, clause := range c.Clauses {
		var cl Node
		cl, err = fun(clause, err)
		theCl, ok := cl.(*CaseClause)
		if !ok {
			log.Panicf("invalid clause: %v", cl)
		}

		c.Clauses[i] = theCl
	}

	return c, err
}

var _ Node = &Case{}

type CaseClause struct {
	Patterns []Node
	Expr     Node
}

func (c CaseClause) String() string {
	var pat fmt.Stringer
	if len(c.Patterns) > 1 {
		pat = utils.Parenthesize("", utils.Concat(c.Patterns))
	} else {
		pat = c.Patterns[0]
	}

	return utils.Parenthesize("clause", pat, c.Expr).String()
}

func (c *CaseClause) Base() token.Token {
	if len(c.Patterns) > 0 {
		return c.Patterns[0].Base()
	}

	return c.Expr.Base()
}

func (c *CaseClause) Plate(err error, f func(Node, error) (Node, error)) (Node, error) {
	for i, pattern := range c.Patterns {
		c.Patterns[i], err = f(pattern, err)
	}
	c.Expr, err = f(c.Expr, err)

	return c, err
}

var _ Node = &CaseClause{}

type Object struct {
	Fields []*Field // len(Fields) > 0
}

func (o Object) String() string {
	return utils.Parenthesize("object", utils.Concat(o.Fields)).String()
}

func (o *Object) Base() token.Token {
	return o.Fields[0].Base()
}

func (o *Object) Plate(err error, f func(Node, error) (Node, error)) (Node, error) {
	for i, field := range o.Fields {
		var fl Node
		fl, err = f(field, err)
		theFl, ok := fl.(*Field)
		if !ok {
			log.Panicf("invalid field: %v", fl)
		}

		o.Fields[i] = theFl
	}

	return o, err
}

var _ Node = &Object{}

type Field struct {
	Name string
	Expr Node
}

func (f Field) String() string {
	return utils.Parenthesize("field "+f.Name, f.Expr).String()
}

func (f *Field) Base() token.Token {
	return f.Expr.Base()
}

func (f *Field) Plate(err error, g func(Node, error) (Node, error)) (Node, error) {
	f.Expr, err = g(f.Expr, err)

	return f, err
}

var _ Node = &Field{}

type TypeDecl struct {
	Def   Node
	Types []Node
}

func (t TypeDecl) String() string {
	return utils.Parenthesize("type", t.Def, utils.Concat(t.Types)).String()
}

func (t *TypeDecl) Base() token.Token {
	return t.Def.Base()
}

func (t *TypeDecl) Plate(err error, f func(Node, error) (Node, error)) (Node, error) {
	t.Def, err = f(t.Def, err)
	for i, typ := range t.Types {
		t.Types[i], err = f(typ, err)
	}

	return t, err
}

var _ Node = &TypeDecl{}

type VarDecl struct {
	Name token.Token
	Type Node
	Expr Node
}

func (v VarDecl) String() string {
	if v.Type == nil {
		return utils.Parenthesize("def", v.Name, v.Expr).String()
	}
	if v.Expr == nil {
		return utils.Parenthesize("def", v.Name, v.Type).String()
	}

	return utils.Parenthesize("def", v.Name, v.Type, v.Expr).String()
}

func (v *VarDecl) Base() token.Token {
	return v.Name
}

func (v *VarDecl) Plate(err error, f func(Node, error) (Node, error)) (Node, error) {
	if v.Type != nil {
		v.Type, err = f(v.Type, err)
	}
	if v.Expr != nil {
		v.Expr, err = f(v.Expr, err)
	}

	return v, err
}

var _ Node = &VarDecl{}

type InfixDecl struct {
	Assoc token.Token
	Prec  token.Token
	Name  token.Token
}

func (i InfixDecl) String() string {
	return utils.Parenthesize("infix", i.Assoc, i.Prec, i.Name).String()
}

func (i *InfixDecl) Base() token.Token {
	return i.Assoc
}

func (i *InfixDecl) Plate(err error, _ func(Node, error) (Node, error)) (Node, error) {
	return i, err
}

var _ Node = &InfixDecl{}

type This struct {
	token.Token
}

func (t This) String() string {
	return "#"
}

func (t *This) Base() token.Token {
	return t.Token
}

func (t *This) Plate(err error, _ func(Node, error) (Node, error)) (Node, error) {
	return t, err
}

var _ Node = &This{}

// Traverse the [Node] in depth-first order.
// f is called for each node.
// If f returns an error, f also must return the original argument n.
// If n is defined in ast.go and has children, Traverse modifies each child before n.
// Otherwise, n is directly applied to f.
//
//tool:ignore
func Traverse(n Node, f func(Node, error) (Node, error)) (Node, error) {
	n, err := n.Plate(nil, func(n Node, _ error) (Node, error) {
		return Traverse(n, f)
	})

	return f(n, err)
}

//tool:ignore
func Children(n Node) []Node {
	var children []Node
	_, err := n.Plate(nil, func(n Node, _ error) (Node, error) {
		children = append(children, n)

		return n, nil
	})
	if err != nil {
		panic(fmt.Errorf("unexpected error: %w", err))
	}

	return children
}

//tool:ignore
func Universe(n Node) []Node {
	var nodes []Node
	_, err := Traverse(n, func(n Node, _ error) (Node, error) {
		nodes = append(nodes, n)

		return n, nil
	})
	if err != nil {
		panic(fmt.Errorf("unexpected error: %w", err))
	}

	return nodes
}
