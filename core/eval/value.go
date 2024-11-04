package eval

import (
	"fmt"
	"strings"

	"github.com/malgo-lang/malgo/core"
	. "github.com/malgo-lang/malgo/pretty"
)

type Value struct {
	Repr  core.Repr
	Trace Trace
}

func (v Value) String() string {
	return v.Pretty(0).String()
}

func (v Value) Pretty(level int, opts ...Option) fmt.Stringer {
	return Braces(v.Repr, v.Trace).Pretty(level, opts...)
}

type Covalue struct {
	Corepr     core.Corepr
	Annotation Annotation
}

func (c Covalue) String() string {
	return c.Pretty(0).String()
}

func (c Covalue) Pretty(level int, opts ...Option) fmt.Stringer {
	return c.Corepr.Pretty(level, opts...)
}

// Trace is a type that represents a trace of evaluation.
type Trace interface {
	Pretty
	isTrace()
}

type Root struct{}

//exhaustruct:ignore
var _ Trace = &Root{}

func (r *Root) String() string {
	return r.Pretty(0).String()
}

func (r *Root) Pretty(level int, opts ...Option) fmt.Stringer {
	return String("#").Pretty(level, opts...)
}

func (r *Root) isTrace() {}

type Construct struct {
	Origin Value
	Name   string
	Args   []Value
	Conts  []Covalue
	Trace  Trace
}

//exhaustruct:ignore
var _ Trace = &Construct{}

func (c *Construct) String() string {
	return c.Pretty(0).String()
}

func (c *Construct) Pretty(level int, opts ...Option) fmt.Stringer {
	return Parens(
		String("construct"),
		c.Origin,
		String(c.Name),
		ParensList(c.Args),
		ParensList(c.Conts),
		c.Trace,
	).Pretty(level, opts...)
}

func (c *Construct) isTrace() {}

type Annotation func(Value) Value

func NoAnnotation(v Value) Value {
	return v
}

func indent(level int) string {
	return strings.Repeat("\t", level)
}
