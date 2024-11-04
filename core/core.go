package core

import (
	"fmt"

	. "github.com/malgo-lang/malgo/pretty"
	"github.com/malgo-lang/malgo/token"
)

type Node interface {
	fmt.Stringer
	Pretty
	Base() token.Token
}

type Producer interface {
	Node
	// If isAtomic returns true, the producer is a atomic node.
	// Atomic nodes can be treated as a value or a variable.
	isAtomic() bool
}

// Repr is a type that represents a representation of a value.
type Repr interface {
	Producer
	isRepr()
}

type Consumer interface {
	Node
	isConsumer()
}

// Corepr is a type that represents a representation of a covalue.
type Corepr interface {
	Consumer
	isCorepr()
}

type Statement interface {
	Node
	isStatement()
}

type Pattern interface {
	Node
	isPattern()
}

type Var struct {
	Name token.Token
}

func (v *Var) String() string {
	return v.Pretty(0).String()
}

func (v *Var) Pretty(level int, opts ...Option) fmt.Stringer {
	return Parens(String("var"), v.Name).Pretty(level, opts...)
}

func (v *Var) Base() token.Token {
	return v.Name
}

func (v *Var) isAtomic() bool {
	return true
}

func (v *Var) isConsumer() {}

func (v *Var) isPattern() {}

//exhaustruct:ignore
var _ Producer = &Var{}

//exhaustruct:ignore
var _ Consumer = &Var{}

//exhaustruct:ignore
var _ Pattern = &Var{}

type Literal struct {
	token.Token
}

func (l *Literal) Pretty(level int, opts ...Option) fmt.Stringer {
	return Parens(String("literal"), l.Token).Pretty(level, opts...)
}

func (l *Literal) Base() token.Token {
	return l.Token
}

func (l *Literal) isAtomic() bool {
	return true
}

func (l *Literal) isRepr() {}

func (l *Literal) isPattern() {}

//exhaustruct:ignore
var _ Producer = &Literal{}

//exhaustruct:ignore
var _ Repr = &Literal{}

//exhaustruct:ignore
var _ Pattern = &Literal{}

type Symbol struct {
	Name token.Token
}

func (s *Symbol) String() string {
	return s.Pretty(0).String()
}

func (s *Symbol) Pretty(level int, opts ...Option) fmt.Stringer {
	return Parens(String("symbol"), s.Name).Pretty(level, opts...)
}

func (s *Symbol) Base() token.Token {
	return s.Name
}

func (s *Symbol) isAtomic() bool {
	return true
}

func (s *Symbol) isRepr() {}

func (s *Symbol) isPattern() {}

//exhaustruct:ignore
var _ Producer = &Symbol{}

//exhaustruct:ignore
var _ Repr = &Symbol{}

//exhaustruct:ignore
var _ Pattern = &Symbol{}

type Destruct struct {
	Name  string
	Args  []Producer
	Conts []Consumer
}

func (d *Destruct) String() string {
	return d.Pretty(0).String()
}

func (d *Destruct) Pretty(level int, opts ...Option) fmt.Stringer {
	return Parens(
		String("destruct"),
		String(d.Name),
		ParensList(d.Args),
		ParensList(d.Conts),
	).Pretty(level, opts...)
}

func (d *Destruct) Base() token.Token {
	if len(d.Args) != 0 {
		return d.Args[0].Base()
	}

	return d.Conts[0].Base()
}

func (d *Destruct) isConsumer() {}

func (d *Destruct) isCorepr() {}

//exhaustruct:ignore
var _ Consumer = &Destruct{}

//exhaustruct:ignore
var _ Corepr = &Destruct{}

// Extract is a pattern corresponds Destruct.
type Extract struct {
	Target Pattern
	Name   string
	Args   []Pattern
	Conts  []Pattern
}

func (e *Extract) String() string {
	return e.Pretty(0).String()
}

func (e *Extract) Pretty(level int, opts ...Option) fmt.Stringer {
	return Parens(
		String("extract"),
		e.Target,
		String(e.Name),
		ParensList(e.Args),
		ParensList(e.Conts),
	).Pretty(level, opts...)
}

func (e *Extract) Base() token.Token {
	return e.Target.Base()
}

func (e *Extract) isPattern() {}

//exhaustruct:ignore
var _ Pattern = &Extract{}

type Prim struct {
	Name token.Token
	Args []Producer
	Cont Consumer
}

func (p *Prim) String() string {
	return p.Pretty(0).String()
}

func (p *Prim) Pretty(level int, opts ...Option) fmt.Stringer {
	return Parens(
		String("prim"),
		p.Name,
		ParensList(p.Args),
		p.Cont,
	).Pretty(level, opts...)
}

func (p *Prim) Base() token.Token {
	return p.Name
}

func (p *Prim) isStatement() {}

//exhaustruct:ignore
var _ Statement = &Prim{}

// μ-abstraction.
type Do struct {
	Name token.Token
	Body Statement
}

func (d *Do) String() string {
	return d.Pretty(0).String()
}

func (d *Do) Pretty(level int, opts ...Option) fmt.Stringer {
	return Parens(
		String("do"),
		d.Name,
		d.Body,
	).Pretty(level, opts...)
}

func (d *Do) Base() token.Token {
	return d.Body.Base()
}

func (d *Do) isAtomic() bool {
	return false
}

//exhaustruct:ignore
var _ Producer = &Do{}

// μ~-abstraction.
type Then struct {
	Name token.Token
	Body Statement
}

func (t *Then) String() string {
	return t.Pretty(0).String()
}

func (t *Then) Pretty(level int, opts ...Option) fmt.Stringer {
	return Parens(
		String("then"),
		t.Name,
		t.Body,
	).Pretty(level, opts...)
}

func (t *Then) Base() token.Token {
	return t.Body.Base()
}

func (t *Then) isConsumer() {}

func (t *Then) isCorepr() {}

//exhaustruct:ignore
var _ Consumer = &Then{}

//exhaustruct:ignore
var _ Corepr = &Then{}

type Cut struct {
	Producer Producer
	Consumer Consumer
}

func (c *Cut) String() string {
	return c.Pretty(0).String()
}

func (c *Cut) Pretty(level int, opts ...Option) fmt.Stringer {
	return Parens(
		String("cut"),
		c.Producer,
		c.Consumer,
	).Pretty(level, opts...)
}

func (c *Cut) Base() token.Token {
	return c.Producer.Base()
}

func (c *Cut) isStatement() {}

//exhaustruct:ignore
var _ Statement = &Cut{}

type Case struct {
	Clauses []*Clause
}

func (c *Case) String() string {
	return c.Pretty(0).String()
}

func (c *Case) Pretty(level int, opts ...Option) fmt.Stringer {
	return Parens(
		String("case"),
		ParensList(c.Clauses),
	).Pretty(level, opts...)
}

func (c *Case) Base() token.Token {
	return c.Clauses[0].Base()
}

func (c *Case) isConsumer() {}

func (c *Case) isCorepr() {}

//exhaustruct:ignore
var _ Consumer = &Case{}

//exhaustruct:ignore
var _ Corepr = &Case{}

type Clause struct {
	Pattern Pattern
	Body    Statement
}

func (c *Clause) String() string {
	return c.Pretty(0).String()
}

func (c *Clause) Pretty(level int, opts ...Option) fmt.Stringer {
	return Parens(
		String("clause"),
		c.Pattern,
		c.Body,
	).Pretty(level, opts...)
}

func (c *Clause) Base() token.Token {
	return c.Body.Base()
}

//exhaustruct:ignore
var _ Node = &Clause{}

type Cocase struct {
	Methods []*Method
}

func (c *Cocase) String() string {
	return c.Pretty(0).String()
}

func (c *Cocase) Pretty(level int, opts ...Option) fmt.Stringer {
	return Parens(
		String("cocase"),
		ParensList(c.Methods),
	).Pretty(level, opts...)
}

func (c *Cocase) Base() token.Token {
	return c.Methods[0].Base()
}

func (c *Cocase) isAtomic() bool {
	return true
}

func (c *Cocase) isRepr() {}

//exhaustruct:ignore
var _ Producer = &Cocase{}

//exhaustruct:ignore
var _ Repr = &Cocase{}

type Method struct {
	Name   string
	Params []token.Token
	Labels []token.Token
	Body   Statement
}

func (m *Method) String() string {
	return m.Pretty(0).String()
}

func (m *Method) Pretty(level int, opts ...Option) fmt.Stringer {
	return Parens(
		String("method"),
		String(m.Name),
		ParensList(m.Params),
		ParensList(m.Labels),
		m.Body,
	).Pretty(level, opts...)
}

func (m *Method) Base() token.Token {
	return m.Body.Base()
}

//exhaustruct:ignore
var _ Node = &Method{}

type Def struct {
	Name    token.Token
	Returns []token.Token
	Body    Statement
}

func (d *Def) String() string {
	return d.Pretty(0).String()
}

func (d *Def) Pretty(level int, opts ...Option) fmt.Stringer {
	return Parens(
		String("def"),
		d.Name,
		ParensList(d.Returns),
		d.Body,
	).Pretty(level, opts...)
}

func (d *Def) Base() token.Token {
	return d.Name
}

//exhaustruct:ignore
var _ Node = &Def{}

type Invoke struct {
	Name  token.Token
	Conts []Consumer
}

func (i *Invoke) String() string {
	return i.Pretty(0).String()
}

func (i *Invoke) Pretty(level int, opts ...Option) fmt.Stringer {
	return Parens(
		String("invoke"),
		i.Name,
		ParensList(i.Conts),
	).Pretty(level, opts...)
}

func (i *Invoke) Base() token.Token {
	return i.Name
}

func (i *Invoke) isStatement() {}

//exhaustruct:ignore
var _ Statement = &Invoke{}

type Toplevel struct{}

func (t *Toplevel) String() string {
	return t.Pretty(0).String()
}

func (t *Toplevel) Pretty(level int, opts ...Option) fmt.Stringer {
	return String("toplevel").Pretty(level, opts...)
}

func (t *Toplevel) Base() token.Token {
	return token.Dummy()
}

func (t *Toplevel) isConsumer() {}

func (t *Toplevel) isCorepr() {}

//exhaustruct:ignore
var _ Consumer = &Toplevel{}

//exhaustruct:ignore
var _ Corepr = &Toplevel{}
