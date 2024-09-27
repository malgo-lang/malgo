package core

import (
	"fmt"

	"github.com/malgo-lang/malgo/token"
	"github.com/malgo-lang/malgo/utils"
)

type Node interface {
	fmt.Stringer
	Base() token.Token
}

type Producer interface {
	Node
	isProducer()
}

type Consumer interface {
	Node
	isConsumer()
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
	return utils.Parenthesize("var", v.Name).String()
}

func (v *Var) Base() token.Token {
	return v.Name
}

func (v *Var) isProducer() {}

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

func (l *Literal) String() string {
	return utils.Parenthesize("literal", l.Token).String()
}

func (l *Literal) Base() token.Token {
	return l.Token
}

func (l *Literal) isProducer() {}

func (l *Literal) isPattern() {}

//exhaustruct:ignore
var _ Producer = &Literal{}

//exhaustruct:ignore
var _ Pattern = &Literal{}

type Symbol struct {
	Name token.Token
}

func (s *Symbol) String() string {
	return utils.Parenthesize("symbol", s.Name).String()
}

func (s *Symbol) Base() token.Token {
	return s.Name
}

func (s *Symbol) isProducer() {}

func (s *Symbol) isPattern() {}

//exhaustruct:ignore
var _ Producer = &Symbol{}

//exhaustruct:ignore
var _ Pattern = &Symbol{}

type Destruct struct {
	Name  string
	Args  []Producer
	Conts []Consumer
}

func (d *Destruct) String() string {
	return fmt.Sprintf(".%s(%v; %v)", d.Name, utils.Concat(d.Args), utils.Concat(d.Conts))
}

func (d *Destruct) Base() token.Token {
	if len(d.Args) != 0 {
		return d.Args[0].Base()
	}

	return d.Conts[0].Base()
}

func (d *Destruct) isConsumer() {}

//exhaustruct:ignore
var _ Consumer = &Destruct{}

// Extract is a pattern corresponds Destruct.
type Extract struct {
	Target Pattern
	Name   string
	Args   []Pattern
}

func (e *Extract) String() string {
	return fmt.Sprintf("%v.%s(%v)", e.Target, e.Name, utils.Concat(e.Args))
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
	return utils.Parenthesize("prim", p.Name, utils.Concat(p.Args), p.Cont).String()
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

func (b *Do) String() string {
	return utils.Parenthesize("μ", b.Name, b.Body).String()
}

func (b *Do) Base() token.Token {
	return b.Body.Base()
}

func (b *Do) isProducer() {}

//exhaustruct:ignore
var _ Producer = &Do{}

// μ~-abstraction.
type Then struct {
	Name token.Token
	Body Statement
}

func (t *Then) String() string {
	return utils.Parenthesize("μ~", t.Name, t.Body).String()
}

func (t *Then) Base() token.Token {
	return t.Body.Base()
}

func (t *Then) isConsumer() {}

//exhaustruct:ignore
var _ Consumer = &Then{}

type Cut struct {
	Producer Producer
	Consumer Consumer
}

func (c *Cut) String() string {
	return fmt.Sprintf("<%v | %v>", c.Producer, c.Consumer)
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
	return utils.Parenthesize("case", utils.Concat(c.Clauses)).String()
}

func (c *Case) Base() token.Token {
	return c.Clauses[0].Base()
}

func (c *Case) isConsumer() {}

//exhaustruct:ignore
var _ Consumer = &Case{}

type Clause struct {
	Patterns []Pattern
	Body     Statement
}

func (c *Clause) String() string {
	var pat fmt.Stringer
	if len(c.Patterns) > 1 {
		pat = utils.Parenthesize("", utils.Concat(c.Patterns))
	} else {
		pat = c.Patterns[0]
	}

	return fmt.Sprintf("%v -> %v)", pat, c.Body)
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
	return utils.Parenthesize("cocase", utils.Concat(c.Methods)).String()
}

func (c *Cocase) Base() token.Token {
	return c.Methods[0].Base()
}

func (c *Cocase) isProducer() {}

//exhaustruct:ignore
var _ Producer = &Cocase{}

type Method struct {
	Name   string
	Params []token.Token
	Labels []token.Token
	Body   Statement
}

func (m *Method) String() string {
	return fmt.Sprintf(".%s(%v; %v) -> %v", m.Name, utils.Concat(m.Params), utils.Concat(m.Labels), m.Body)
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
	return utils.Parenthesize("def", d.Name, utils.Concat(d.Returns), d.Body).String()
}

func (d *Def) Base() token.Token {
	return d.Name
}

//exhaustruct:ignore
var _ Node = &Def{}
