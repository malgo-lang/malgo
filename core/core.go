package core

import (
	"fmt"
	"strings"

	"github.com/malgo-lang/malgo/token"
	"github.com/malgo-lang/malgo/utils"
)

type PrettyOpts struct {
	Header string
}

func DefaultPrettyOpts() *PrettyOpts {
	return &PrettyOpts{
		Header: "",
	}
}

type Option func(*PrettyOpts)

func WithHeader(header string) Option {
	return func(o *PrettyOpts) {
		o.Header = header
	}
}

type Pretty interface {
	fmt.Stringer
	Pretty(level int, opts ...Option) string
}

func Indent(level int) string {
	return strings.Repeat(" ", level)
}

const tabSize int = 2

const multilineThreshold int = 16

func shouldMultiline[T Node](nodes []T) bool {
	for _, node := range nodes {
		str := node.Pretty(0)

		if strings.Contains(str, "\n") {
			return true
		}

		if len(str) > multilineThreshold {
			return true
		}
	}

	return false
}

type Node interface {
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
	return v.Pretty(0)
}

func (v *Var) Pretty(level int, opts ...Option) string {
	o := DefaultPrettyOpts()

	for _, opt := range opts {
		opt(o)
	}

	return fmt.Sprintf("%s%s%v", Indent(level), o.Header, v.Name)
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

func (l *Literal) Pretty(level int, opts ...Option) string {
	o := DefaultPrettyOpts()

	for _, opt := range opts {
		opt(o)
	}

	return fmt.Sprintf("%s%s%v", Indent(level), o.Header, l.Token)
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
	return s.Pretty(0)
}

func (s *Symbol) Pretty(level int, opts ...Option) string {
	o := DefaultPrettyOpts()

	for _, opt := range opts {
		opt(o)
	}

	if s.Name.Lexeme[0] == ':' {
		return fmt.Sprintf("%s%s%s", Indent(level), o.Header, s.Name.Lexeme)
	}

	return fmt.Sprintf("%s%s:%s", Indent(level), o.Header, s.Name.Lexeme)
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
	return d.Pretty(0)
}

func (d *Destruct) Pretty(level int, opts ...Option) string {
	o := DefaultPrettyOpts()

	for _, opt := range opts {
		opt(o)
	}

	var builder strings.Builder
	isMultiline := false

	fmt.Fprintf(&builder, "%s%s.%s(", Indent(level), o.Header, d.Name)
	if shouldMultiline(d.Args) {
		isMultiline = true
		for i, arg := range d.Args {
			if i == 0 {
				fmt.Fprintf(&builder, "\n%v", arg.Pretty(level+tabSize+len(o.Header)))
			} else {
				fmt.Fprintf(&builder, ",\n%v", arg.Pretty(level+tabSize+len(o.Header)))
			}
		}
	} else {
		for i, arg := range d.Args {
			if i == 0 {
				fmt.Fprintf(&builder, "%v", arg.Pretty(0))
			} else {
				fmt.Fprintf(&builder, ", %v", arg.Pretty(0))
			}
		}
	}

	fmt.Fprintf(&builder, "; ")

	if isMultiline || shouldMultiline(d.Conts) {
		for i, cont := range d.Conts {
			if i == 0 {
				fmt.Fprintf(&builder, "\n%v", cont.Pretty(level+tabSize+len(o.Header)))
			} else {
				fmt.Fprintf(&builder, ",\n%v", cont.Pretty(level+tabSize+len(o.Header)))
			}
		}
	} else {
		for i, cont := range d.Conts {
			if i == 0 {
				fmt.Fprintf(&builder, "%v", cont.Pretty(0))
			} else {
				fmt.Fprintf(&builder, ", %v", cont.Pretty(0))
			}
		}
	}

	fmt.Fprintf(&builder, ")")

	return builder.String()
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
	Conts  []Pattern
}

func (e *Extract) String() string {
	return e.Pretty(0)
}

func (e *Extract) Pretty(level int, opts ...Option) string {
	o := DefaultPrettyOpts()

	for _, opt := range opts {
		opt(o)
	}

	var builder strings.Builder
	isMultiline := false

	fmt.Fprintf(&builder, "%s.%s(", e.Target.Pretty(level, opts...), e.Name)

	if shouldMultiline(e.Args) {
		isMultiline = true
		for i, arg := range e.Args {
			if i == 0 {
				fmt.Fprintf(&builder, "\n%s", arg.Pretty(level+tabSize+len(o.Header)))
			} else {
				fmt.Fprintf(&builder, ",\n%s", arg.Pretty(level+tabSize+len(o.Header)))
			}
		}
	} else {
		for i, arg := range e.Args {
			if i == 0 {
				fmt.Fprintf(&builder, "%s", arg.Pretty(0))
			} else {
				fmt.Fprintf(&builder, ", %s", arg.Pretty(0))
			}
		}
	}

	fmt.Fprintf(&builder, ";")

	if isMultiline || shouldMultiline(e.Conts) {
		for i, cont := range e.Conts {
			if i == 0 {
				fmt.Fprintf(&builder, "\n%s", cont.Pretty(level+tabSize+len(o.Header)))
			} else {
				fmt.Fprintf(&builder, ",\n%s", cont.Pretty(level+tabSize+len(o.Header)))
			}
		}
	} else {
		for i, cont := range e.Conts {
			if i == 0 {
				fmt.Fprintf(&builder, " %s", cont.Pretty(0))
			} else {
				fmt.Fprintf(&builder, ", %s", cont.Pretty(0))
			}
		}
	}

	fmt.Fprintf(&builder, ")")

	return builder.String()
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
	return p.Pretty(0)
}

func (p *Prim) Pretty(level int, opts ...Option) string {
	o := DefaultPrettyOpts()

	for _, opt := range opts {
		opt(o)
	}

	var builder strings.Builder
	isMultiline := false

	fmt.Fprintf(&builder, "%s%sprim %v(", Indent(level), o.Header, p.Name)

	if shouldMultiline(p.Args) {
		isMultiline = true
		for i, arg := range p.Args {
			if i == 0 {
				fmt.Fprintf(&builder, "\n%s", arg.Pretty(level+tabSize+len(o.Header)))
			} else {
				fmt.Fprintf(&builder, ",\n%s", arg.Pretty(level+tabSize+len(o.Header)))
			}
		}
	} else {
		for i, arg := range p.Args {
			if i == 0 {
				fmt.Fprintf(&builder, "%s", arg.Pretty(0))
			} else {
				fmt.Fprintf(&builder, ", %s", arg.Pretty(0))
			}
		}
	}

	fmt.Fprintf(&builder, ";")

	if isMultiline || shouldMultiline([]Consumer{p.Cont}) {
		fmt.Fprintf(&builder, "\n%s", p.Cont.Pretty(level+tabSize+len(o.Header)))
	} else {
		fmt.Fprintf(&builder, " %s", p.Cont.Pretty(0))
	}

	fmt.Fprintf(&builder, ")")

	return builder.String()
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
	return d.Pretty(0)
}

func (d *Do) Pretty(level int, opts ...Option) string {
	o := DefaultPrettyOpts()

	for _, opt := range opts {
		opt(o)
	}

	var builder strings.Builder

	fmt.Fprintf(&builder, "%s%sμ %v:\n", Indent(level), o.Header, d.Name)
	fmt.Fprintf(&builder, "%s", d.Body.Pretty(level+tabSize+len(o.Header)))

	return builder.String()
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
	return t.Pretty(0)
}

func (t *Then) Pretty(level int, opts ...Option) string {
	o := DefaultPrettyOpts()

	for _, opt := range opts {
		opt(o)
	}

	var builder strings.Builder

	fmt.Fprintf(&builder, "%s%sμ~ %v:\n", Indent(level), o.Header, t.Name)
	fmt.Fprintf(&builder, "%s", t.Body.Pretty(level+tabSize+len(o.Header)))

	return builder.String()
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
	return c.Pretty(0)
}

func (c *Cut) Pretty(level int, opts ...Option) string {
	o := DefaultPrettyOpts()

	for _, opt := range opts {
		opt(o)
	}

	return fmt.Sprintf("%s\n%s", c.Producer.Pretty(level), c.Consumer.Pretty(level, WithHeader("| ")))
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
	return c.Pretty(0)
}

func (c *Case) Pretty(level int, opts ...Option) string {
	o := DefaultPrettyOpts()

	for _, opt := range opts {
		opt(o)
	}

	var builder strings.Builder

	fmt.Fprintf(&builder, "%s%scase", Indent(level), o.Header)

	for _, clause := range c.Clauses {
		fmt.Fprintf(&builder, "\n%s", clause.Pretty(level+tabSize+len(o.Header)))
	}

	return builder.String()
}

func (c *Case) Base() token.Token {
	return c.Clauses[0].Base()
}

func (c *Case) isConsumer() {}

//exhaustruct:ignore
var _ Consumer = &Case{}

type Clause struct {
	Pattern Pattern
	Body    Statement
}

func (c *Clause) String() string {
	return c.Pretty(0)
}

func (c *Clause) Pretty(level int, opts ...Option) string {
	o := DefaultPrettyOpts()

	for _, opt := range opts {
		opt(o)
	}

	var builder strings.Builder

	fmt.Fprintf(&builder, "%s ->\n", c.Pattern.Pretty(level+len(o.Header)))
	fmt.Fprintf(&builder, "%s", c.Body.Pretty(level+tabSize+len(o.Header)))

	return builder.String()
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
	return c.Pretty(0)
}

func (c *Cocase) Pretty(level int, opts ...Option) string {
	o := DefaultPrettyOpts()

	for _, opt := range opts {
		opt(o)
	}

	var builder strings.Builder

	fmt.Fprintf(&builder, "%s%scocase", Indent(level), o.Header)

	for _, method := range c.Methods {
		fmt.Fprintf(&builder, "\n%s", method.Pretty(level+tabSize+len(o.Header)))
	}

	return builder.String()
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
	return m.Pretty(0)
}

func (m *Method) Pretty(level int, opts ...Option) string {
	o := DefaultPrettyOpts()

	for _, opt := range opts {
		opt(o)
	}

	var builder strings.Builder

	fmt.Fprintf(&builder, "%s%s.%s(", Indent(level), o.Header, m.Name)
	for i, param := range m.Params {
		if i == 0 {
			fmt.Fprintf(&builder, "%v", param)
		} else {
			fmt.Fprintf(&builder, ", %v", param)
		}
	}
	fmt.Fprintf(&builder, "; ")
	for i, label := range m.Labels {
		if i == 0 {
			fmt.Fprintf(&builder, "%v", label)
		} else {
			fmt.Fprintf(&builder, ", %v", label)
		}
	}
	fmt.Fprintf(&builder, ") ->\n")

	fmt.Fprintf(&builder, "%s", m.Body.Pretty(level+tabSize+len(o.Header)))

	return builder.String()
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
	return d.Pretty(0)
}

func (d *Def) Pretty(level int, opts ...Option) string {
	o := DefaultPrettyOpts()

	for _, opt := range opts {
		opt(o)
	}

	return fmt.Sprintf(
		"%s%sdef %v(%v)\n%s",
		Indent(level),
		o.Header,
		d.Name,
		utils.Concat(d.Returns),
		d.Body.Pretty(level+tabSize+len(o.Header)),
	)
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
	return i.Pretty(0)
}

func (i *Invoke) Pretty(level int, opts ...Option) string {
	o := DefaultPrettyOpts()

	for _, opt := range opts {
		opt(o)
	}

	var builder strings.Builder

	fmt.Fprintf(&builder, "%s%sinvoke %v(", Indent(level), o.Header, i.Name)

	if shouldMultiline(i.Conts) {
		for i, cont := range i.Conts {
			if i == 0 {
				fmt.Fprintf(&builder, "\n%s", cont.Pretty(level+tabSize+len(o.Header)))
			} else {
				fmt.Fprintf(&builder, ",\n%s", cont.Pretty(level+tabSize+len(o.Header)))
			}
		}
	} else {
		for i, cont := range i.Conts {
			if i == 0 {
				fmt.Fprintf(&builder, "%s", cont.Pretty(0))
			} else {
				fmt.Fprintf(&builder, ", %s", cont.Pretty(0))
			}
		}
	}

	fmt.Fprintf(&builder, ")")

	return builder.String()
}

func (i *Invoke) Base() token.Token {
	return i.Name
}

func (i *Invoke) isStatement() {}

//exhaustruct:ignore
var _ Statement = &Invoke{}

type Toplevel struct{}

func (t *Toplevel) String() string {
	return t.Pretty(0)
}

func (t *Toplevel) Pretty(level int, opts ...Option) string {
	o := DefaultPrettyOpts()

	for _, opt := range opts {
		opt(o)
	}

	return fmt.Sprintf("%s%s", Indent(level), o.Header)
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
